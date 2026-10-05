//
//  SpectrumGraph.swift
//  iFITS Start
//
//  The spectrum plot: value against the spectral axis (in GHz, km/s, µm, …), with an orange line
//  on the channel shown in the image. Drag the orange line (or tap) to change channel; pinch to
//  zoom along the spectral axis; drag elsewhere to pan when zoomed; double-tap to zoom back out.
//

import SwiftUI

/// Round values along the spectral axis between two channels, in a readable unit.
enum SpectrumAxisTicks {
    /// - lower, upper: fractional 0-based channels (a channel's edges are at ±0.5).
    static func make(axis: CubeAxis, lower: Double, upper: Double,
                     target: Int) -> (title: String, ticks: [(channel: Double, text: String)]) {
        if axis.kind == .stokes {
            let a = max(0, Int(ceil(lower))), b = min(axis.length - 1, Int(floor(upper)))
            guard a <= b else { return ("Stokes", []) }
            return ("Stokes", (a...b).map { i in
                (channel: Double(i), text: axis.summary(at: i).replacingOccurrences(of: "Stokes ", with: ""))
            })
        }
        let (factor, unit, name) = ARAxisTicks.displayUnit(axis)
        guard axis.cdelt != 0, factor != 0, factor.isFinite else {
            let (values, step) = NiceTicks.values(lo: max(0, lower), hi: upper, target: target)
            let whole = values.filter { $0 == $0.rounded() }
            return ("Channel", whole.map { (channel: $0, text: NiceTicks.label($0, step: max(1, step))) })
        }
        // World value (display unit) at a fractional channel.
        func world(_ channel: Double) -> Double {
            (axis.crval + (channel + 1 - axis.crpix) * axis.cdelt) * factor
        }
        let a = world(lower), b = world(upper)
        let (values, step) = NiceTicks.values(lo: min(a, b), hi: max(a, b), target: target)
        let ticks = values.compactMap { v -> (channel: Double, text: String)? in
            let channel = (v / factor - axis.crval) / axis.cdelt + axis.crpix - 1
            guard channel >= lower - 1e-6, channel <= upper + 1e-6 else { return nil }
            return (channel: channel, text: NiceTicks.label(v, step: step))
        }
        return (unit.isEmpty ? name : "\(name) (\(unit))", ticks)
    }
}

struct SpectrumGraph: View {
    /// One value per channel (NaN = no data there); nil while there's nothing to show.
    let values: [Double]?
    let axis: CubeAxis
    /// "Mean (Jy/beam)".
    let yTitle: String
    /// The channel shown in the image (the orange line).
    let current: Int
    /// Visible channel range (nil = all).
    @Binding var zoom: ClosedRange<Double>?
    /// Small version for the top-right box: fewer ticks, no axis titles.
    var compact = false
    /// Shown in the middle while there are no values.
    var placeholder: String? = nil
    var onChannel: (Int) -> Void

    private enum DragMode { case scrub, pan, ignore }

    private struct PinchStart {
        let range: ClosedRange<Double>
        /// Channel under the fingers, and where it is across the plot (0…1).
        let anchor: Double
        let fraction: Double
    }

    @State private var dragMode: DragMode? = nil
    @State private var dragStartRange: ClosedRange<Double>? = nil
    @State private var channelBeforeTouch: Int? = nil
    @State private var pinchStart: PinchStart? = nil
    /// The last tap, to spot a double-tap (zoom out).
    @State private var lastTap: (time: Date, x: CGFloat, channelBefore: Int)? = nil

    private var channelCount: Int { max(1, axis.length) }
    private var fullRange: ClosedRange<Double> { -0.5...(Double(channelCount) - 0.5) }
    private var visible: ClosedRange<Double> { zoom ?? fullRange }

    private var insets: EdgeInsets {
        compact ? EdgeInsets(top: 6, leading: 46, bottom: 20, trailing: 8)
                : EdgeInsets(top: 10, leading: 66, bottom: 40, trailing: 14)
    }

    var body: some View {
        GeometryReader { geo in
            let plot = plotRect(geo.size)
            ZStack {
                Canvas { context, size in
                    draw(in: &context, size: size, plot: plot)
                }
                if values == nil, let placeholder {
                    Text(placeholder)
                        .font(compact ? .caption : .callout)
                        .foregroundStyle(.secondary)
                        .multilineTextAlignment(.center)
                        .frame(width: max(0, plot.width - 16))
                        .position(x: plot.midX, y: plot.midY)
                }
            }
            .contentShape(Rectangle())
            .gesture(dragGesture(plot))
            .simultaneousGesture(pinchGesture(plot))
        }
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("Spectrum, \(yTitle)")
        .accessibilityValue("\(axis.name) \(current), \(axis.summary(at: current))")
        .accessibilityAdjustableAction { direction in
            switch direction {
            case .increment: onChannel(min(current + 1, channelCount - 1))
            case .decrement: onChannel(max(current - 1, 0))
            @unknown default: break
            }
        }
    }

    // MARK: Geometry

    private func plotRect(_ size: CGSize) -> CGRect {
        CGRect(x: insets.leading, y: insets.top,
               width: max(1, size.width - insets.leading - insets.trailing),
               height: max(1, size.height - insets.top - insets.bottom))
    }

    private func xPosition(_ channel: Double, _ plot: CGRect) -> CGFloat {
        let v = visible
        let span = max(1e-9, v.upperBound - v.lowerBound)
        return plot.minX + CGFloat((channel - v.lowerBound) / span) * plot.width
    }

    private func channel(atX x: CGFloat, _ plot: CGRect) -> Int {
        let v = visible
        let c = v.lowerBound + Double((x - plot.minX) / plot.width) * (v.upperBound - v.lowerBound)
        return min(max(Int(c.rounded()), 0), channelCount - 1)
    }

    /// Channels whose points are on screen.
    private var visibleChannels: ClosedRange<Int>? {
        let a = max(0, Int(ceil(visible.lowerBound))), b = min(channelCount - 1, Int(floor(visible.upperBound)))
        return a <= b ? a...b : nil
    }

    /// Value range of the visible channels, with a little room above and below.
    private func valueRange(_ values: [Double]) -> (lo: Double, hi: Double)? {
        guard let channels = visibleChannels else { return nil }
        var lo = Double.infinity, hi = -Double.infinity
        for k in channels where k < values.count {
            let v = values[k]
            guard v.isFinite else { continue }
            if v < lo { lo = v }
            if v > hi { hi = v }
        }
        guard lo <= hi else { return nil }
        if hi == lo {
            let d = lo == 0 ? 1 : abs(lo) * 0.1
            return (lo - d, hi + d)
        }
        let pad = (hi - lo) * 0.06
        return (lo - pad, hi + pad)
    }

    // MARK: Drawing

    private func draw(in context: inout GraphicsContext, size: CGSize, plot: CGRect) {
        let tickFont: Font = compact ? .system(size: 9).monospacedDigit() : .caption2.monospacedDigit()
        let gridColor = Color.primary.opacity(0.09)
        let range = values.flatMap { valueRange($0) }

        // Spectral axis ticks and grid.
        let xTicks = SpectrumAxisTicks.make(axis: axis, lower: visible.lowerBound, upper: visible.upperBound,
                                            target: compact ? 3 : 6)
        var lastLabelRight = -CGFloat.infinity
        // Left to right (a decreasing axis, e.g. negative CDELT, gives them right to left).
        for tick in xTicks.ticks.sorted(by: { $0.channel < $1.channel }) {
            let x = xPosition(tick.channel, plot)
            guard x >= plot.minX - 0.5, x <= plot.maxX + 0.5 else { continue }
            var line = Path()
            line.move(to: CGPoint(x: x, y: plot.minY))
            line.addLine(to: CGPoint(x: x, y: plot.maxY))
            context.stroke(line, with: .color(gridColor), lineWidth: 1)
            let label = context.resolve(Text(tick.text).font(tickFont).foregroundStyle(.secondary))
            let w = label.measure(in: CGSize(width: 200, height: 40)).width
            let left = min(max(x - w / 2, plot.minX - insets.leading + 2), size.width - w - 2)
            guard left > lastLabelRight + 6 else { continue }
            context.draw(label, at: CGPoint(x: left, y: plot.maxY + 3), anchor: .topLeading)
            lastLabelRight = left + w
        }

        // Value axis ticks and grid.
        if let range {
            let (values, step) = NiceTicks.values(lo: range.lo, hi: range.hi, target: compact ? 3 : 4)
            for v in values {
                let y = plot.maxY - CGFloat((v - range.lo) / (range.hi - range.lo)) * plot.height
                var line = Path()
                line.move(to: CGPoint(x: plot.minX, y: y))
                line.addLine(to: CGPoint(x: plot.maxX, y: y))
                context.stroke(line, with: .color(gridColor), lineWidth: 1)
                let label = context.resolve(Text(NiceTicks.label(v, step: step)).font(tickFont).foregroundStyle(.secondary))
                context.draw(label, at: CGPoint(x: plot.minX - 5, y: y), anchor: .trailing)
            }
        }

        // Frame.
        context.stroke(Path(plot), with: .color(.primary.opacity(0.3)), lineWidth: 1)

        // Axis titles.
        if !compact {
            let xTitle = context.resolve(Text(xTicks.title).font(.caption.weight(.semibold)))
            context.draw(xTitle, at: CGPoint(x: plot.midX, y: size.height - 2), anchor: .bottom)
            var rotated = context
            rotated.translateBy(x: 10, y: plot.midY)
            rotated.rotate(by: .degrees(-90))
            rotated.draw(Text(yTitle).font(.caption.weight(.semibold)), at: .zero, anchor: .center)
        }

        // The spectrum, clipped to the plot.
        if let values, let range, let channels = visibleChannels {
            var plotLayer = context
            plotLayer.clip(to: Path(plot))
            func y(_ v: Double) -> CGFloat {
                plot.maxY - CGFloat((v - range.lo) / (range.hi - range.lo)) * plot.height
            }
            // One channel either side, so the line runs to the edges when zoomed.
            let a = max(0, channels.lowerBound - 1), b = min(values.count - 1, channels.upperBound + 1)
            var path = Path()
            var penDown = false
            if a <= b {
                for k in a...b {
                    let v = values[k]
                    guard v.isFinite else { penDown = false; continue }
                    let p = CGPoint(x: xPosition(Double(k), plot), y: y(v))
                    if penDown { path.addLine(to: p) } else { path.move(to: p) }
                    penDown = true
                }
            }
            plotLayer.stroke(path, with: .color(.primary), style: StrokeStyle(lineWidth: compact ? 1.2 : 1.5, lineJoin: .round))

            // Dots once the channels are far apart.
            let spacing = plot.width / CGFloat(max(1e-9, visible.upperBound - visible.lowerBound))
            if spacing >= 10, a <= b {
                for k in a...b where values[k].isFinite {
                    let p = CGPoint(x: xPosition(Double(k), plot), y: y(values[k]))
                    plotLayer.fill(Path(ellipseIn: CGRect(x: p.x - 2, y: p.y - 2, width: 4, height: 4)), with: .color(.primary))
                }
            }
        }

        // The current channel: an orange line with a handle, and a dot on the spectrum.
        let cx = xPosition(Double(current), plot)
        if cx >= plot.minX - 0.5, cx <= plot.maxX + 0.5 {
            var line = Path()
            line.move(to: CGPoint(x: cx, y: plot.minY))
            line.addLine(to: CGPoint(x: cx, y: plot.maxY))
            context.stroke(line, with: .color(.orange), lineWidth: 2)
            let handle = CGRect(x: cx - 5, y: plot.minY - (compact ? 3 : 5), width: 10, height: compact ? 8 : 12)
            context.fill(Path(roundedRect: handle, cornerRadius: 3), with: .color(.orange))
            if let values, let range, values.indices.contains(current), values[current].isFinite {
                let cy = plot.maxY - CGFloat((values[current] - range.lo) / (range.hi - range.lo)) * plot.height
                let dot = CGRect(x: cx - 4, y: cy - 4, width: 8, height: 8)
                context.fill(Path(ellipseIn: dot), with: .color(.orange))
                context.stroke(Path(ellipseIn: dot), with: .color(.white), lineWidth: 1.2)
            }
        }
    }

    // MARK: Gestures

    /// Drag the orange line (or anywhere, when not zoomed) to change channel; drag elsewhere to pan
    /// when zoomed. A tap moves the line there.
    private func dragGesture(_ plot: CGRect) -> some Gesture {
        DragGesture(minimumDistance: 0)
            .onChanged { value in
                if dragMode == nil {
                    channelBeforeTouch = current
                    dragStartRange = visible
                    let nearLine = abs(value.startLocation.x - xPosition(Double(current), plot)) < (compact ? 16 : 24)
                    dragMode = (zoom == nil || nearLine) ? .scrub : .pan
                }
                switch dragMode ?? .ignore {
                case .scrub:
                    let c = channel(atX: value.location.x, plot)
                    if c != current { onChannel(c) }
                case .pan:
                    guard let start = dragStartRange else { return }
                    let span = start.upperBound - start.lowerBound
                    let shift = -Double(value.translation.width / plot.width) * span
                    let full = fullRange
                    let lower = min(max(start.lowerBound + shift, full.lowerBound), full.upperBound - span)
                    zoom = lower...(lower + span)
                case .ignore:
                    break
                }
            }
            .onEnded { value in
                let isTap = dragMode != .ignore && hypot(value.translation.width, value.translation.height) < 4
                if isTap {
                    let now = Date()
                    if let last = lastTap, now.timeIntervalSince(last.time) < 0.35, abs(last.x - value.location.x) < 30 {
                        // Double-tap: zoom back out, with the line where it was before the taps.
                        zoom = nil
                        if last.channelBefore != current { onChannel(last.channelBefore) }
                        lastTap = nil
                    } else {
                        lastTap = (now, value.location.x, channelBeforeTouch ?? current)
                        // A tap while zoomed (away from the line): move the line there.
                        if dragMode == .pan { onChannel(channel(atX: value.location.x, plot)) }
                    }
                } else {
                    lastTap = nil
                }
                dragMode = nil
                dragStartRange = nil
                channelBeforeTouch = nil
            }
    }

    /// Pinch (or trackpad pinch) to zoom along the spectral axis, around the fingers.
    private func pinchGesture(_ plot: CGRect) -> some Gesture {
        MagnifyGesture(minimumScaleDelta: 0.01)
            .onChanged { value in
                if pinchStart == nil {
                    // The first finger's touch already moved the line: put it back.
                    if dragMode != nil {
                        if dragMode == .scrub, let c = channelBeforeTouch, c != current { onChannel(c) }
                        if let start = dragStartRange, dragMode == .pan { zoom = start == fullRange ? nil : start }
                        dragMode = .ignore
                    }
                    let v = visible
                    let fraction = Double(min(max((value.startLocation.x - plot.minX) / plot.width, 0), 1))
                    pinchStart = PinchStart(range: v, anchor: v.lowerBound + fraction * (v.upperBound - v.lowerBound),
                                            fraction: fraction)
                }
                guard let start = pinchStart else { return }
                let full = fullRange
                let fullSpan = full.upperBound - full.lowerBound
                let minSpan = min(fullSpan, 4)
                let startSpan = start.range.upperBound - start.range.lowerBound
                let span = min(max(startSpan / max(Double(value.magnification), 0.01), minSpan), fullSpan)
                if span >= fullSpan - 1e-9 {
                    zoom = nil
                    return
                }
                let lower = min(max(start.anchor - start.fraction * span, full.lowerBound), full.upperBound - span)
                zoom = lower...(lower + span)
            }
            .onEnded { _ in
                pinchStart = nil
            }
    }
}
