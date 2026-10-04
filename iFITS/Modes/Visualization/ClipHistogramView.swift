//
//  ClipHistogramView.swift
//  iFITS Start
//
//  Histogram with draggable clip lines.
//

import SwiftUI

struct ClipHistogramView: View {
    let stats: ImageStats
    let settings: RenderSettings
    var onClipChange: (Double, Double) -> Void

    private enum Handle { case min, max }
    @State private var activeHandle: Handle?

    private var lo: Double { stats.histLo }
    private var hi: Double { stats.histHi }

    var body: some View {
        GeometryReader { geo in
            Canvas { context, size in
                draw(context, size: size)
            }
            .background(Color.black.opacity(0.3), in: RoundedRectangle(cornerRadius: 10, style: .continuous))
            .clipShape(RoundedRectangle(cornerRadius: 10, style: .continuous))
            .contentShape(Rectangle())
            .gesture(
                DragGesture(minimumDistance: 0)
                    .onChanged { g in handleDrag(g, width: geo.size.width) }
                    .onEnded { _ in activeHandle = nil }
            )
        }
        .accessibilityElement()
        .accessibilityLabel("Histogram. Clip min \(NumberField.format(settings.clipMin)), clip max \(NumberField.format(settings.clipMax))")
    }

    private func xPosition(_ v: Double, width: CGFloat) -> CGFloat {
        let x = CGFloat((v - lo) / (hi - lo)) * width
        return min(max(x, 0), width)
    }

    private func value(atX x: CGFloat, width: CGFloat) -> Double {
        lo + Double(min(max(x / max(width, 1), 0), 1)) * (hi - lo)
    }

    private func handleDrag(_ g: DragGesture.Value, width: CGFloat) {
        if activeHandle == nil {
            // Grab whichever line is closest to where the finger/pointer went down.
            let xMin = xPosition(settings.clipMin, width: width)
            let xMax = xPosition(settings.clipMax, width: width)
            let dMin = abs(g.startLocation.x - xMin), dMax = abs(g.startLocation.x - xMax)
            if abs(dMin - dMax) < 0.5 {
                activeHandle = g.startLocation.x < xMin ? .min : .max
            } else {
                activeHandle = dMin < dMax ? .min : .max
            }
        }
        let v = value(atX: g.location.x, width: width)
        let eps = (hi - lo) * 1e-4
        switch activeHandle {
        case .min: onClipChange(min(v, settings.clipMax - eps), settings.clipMax)
        case .max: onClipChange(settings.clipMin, max(v, settings.clipMin + eps))
        case nil: break
        }
    }

    private func draw(_ context: GraphicsContext, size: CGSize) {
        let w = size.width, h = size.height
        let bins = stats.bins
        guard !bins.isEmpty, w > 0, h > 0 else { return }

        // Histogram (log counts), as a filled step plot.
        let logs = bins.map { log10(Double($0) + 1) }
        let peak = max(logs.max() ?? 1, 1e-9)
        let bw = w / CGFloat(bins.count)
        var step = Path()
        step.move(to: CGPoint(x: 0, y: h))
        for (i, l) in logs.enumerated() {
            let y = h - CGFloat(l / peak) * (h - 10)
            step.addLine(to: CGPoint(x: CGFloat(i) * bw, y: y))
            step.addLine(to: CGPoint(x: CGFloat(i + 1) * bw, y: y))
        }
        step.addLine(to: CGPoint(x: w, y: h))
        context.fill(step, with: .color(.cyan.opacity(0.18)))
        context.stroke(step, with: .color(.cyan), lineWidth: 1.2)

        let xMin = xPosition(settings.clipMin, width: w)
        let xMax = xPosition(settings.clipMax, width: w)

        // Dim everything outside the clip range.
        context.fill(Path(CGRect(x: 0, y: 0, width: xMin, height: h)), with: .color(.black.opacity(0.35)))
        context.fill(Path(CGRect(x: xMax, y: 0, width: w - xMax, height: h)), with: .color(.black.opacity(0.35)))

        // Scaling curve between the clip lines.
        var curve = Path()
        let n = 64
        for i in 0...n {
            let t = Double(i) / Double(n)
            let yv = settings.scaling.apply(t, alpha: settings.alpha, gamma: settings.gamma)
            let yc = yv.isFinite ? min(max(yv, 0), 1) : 0
            let p = CGPoint(x: xMin + (xMax - xMin) * CGFloat(t), y: h - CGFloat(yc) * (h - 4) - 2)
            if i == 0 { curve.move(to: p) } else { curve.addLine(to: p) }
        }
        context.stroke(curve, with: .color(.white.opacity(0.6)), style: StrokeStyle(lineWidth: 1, dash: [4, 3]))

        // Clip lines with grab handles.
        for x in [xMin, xMax] {
            var line = Path()
            line.move(to: CGPoint(x: x, y: 0))
            line.addLine(to: CGPoint(x: x, y: h))
            context.stroke(line, with: .color(.red), lineWidth: 2)
            let knob = CGRect(x: x - 6, y: h / 2 - 16, width: 12, height: 32)
            context.fill(Path(roundedRect: knob, cornerRadius: 6), with: .color(.red))
            var grip = Path()
            grip.move(to: CGPoint(x: x, y: h / 2 - 8))
            grip.addLine(to: CGPoint(x: x, y: h / 2 + 8))
            context.stroke(grip, with: .color(.white.opacity(0.8)), lineWidth: 1.5)
        }
    }
}
