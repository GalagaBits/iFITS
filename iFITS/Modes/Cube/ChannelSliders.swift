//
//  ChannelSliders.swift
//  iFITS Start
//
//  Channel slider, range slider and tick marks.
//

import SwiftUI

/// Which channel numbers get labels and tick marks on a channel slider.
enum ChannelTicks {
    private static let niceSteps = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500,
                                    5000, 10_000, 20_000, 25_000, 50_000, 100_000]

    /// Spacing of labelled channels: about one label per 60 points.
    static func labelStep(count: Int, width: CGFloat) -> Int {
        let last = max(count - 1, 1)
        let maxLabels = max(2, Int(width / 60))
        return niceSteps.first { last / $0 + 1 <= maxLabels } ?? (last / maxLabels + 1)
    }

    /// 0, step, 2·step, …, plus the last channel (like CARTA's 0 5 10 15 20 23).
    static func labels(count: Int, width: CGFloat) -> [Int] {
        guard count > 1 else { return [0] }
        let step = labelStep(count: count, width: width)
        let last = count - 1
        var out = Array(stride(from: 0, to: last, by: step))
        if out.count > 1, let tail = out.last, Double(last - tail) < Double(step) * 0.5 { out.removeLast() }
        out.append(last)
        return out
    }

    /// Spacing of the small ticks: every channel when there's room, otherwise fewer.
    static func minorStep(count: Int, width: CGFloat, labelStep: Int) -> Int {
        let room = width / 6
        for s in niceSteps where s <= labelStep && labelStep % s == 0 && CGFloat(count) / CGFloat(s) <= room {
            return s
        }
        return labelStep
    }
}

/// A slider that locks onto whole channels, with tick marks, channel labels and a value tag.
struct ChannelSlider: View {
    @Binding var value: Int
    let count: Int
    var label = "Channel"

    @State private var isDragging = false
    private let thumb: CGFloat = 22
    private let trackY: CGFloat = 12

    var body: some View {
        GeometryReader { geo in
            let inset = thumb / 2
            let usable = max(1, geo.size.width - thumb)
            let last = CGFloat(max(count - 1, 1))
            let x: (Int) -> CGFloat = { inset + usable * CGFloat($0) / last }
            let labels = ChannelTicks.labels(count: count, width: usable)
            let step = ChannelTicks.labelStep(count: count, width: usable)
            let minor = ChannelTicks.minorStep(count: count, width: usable, labelStep: step)

            ZStack(alignment: .topLeading) {
                Canvas { context, _ in
                    let track = CGRect(x: inset, y: trackY - 2, width: usable, height: 4)
                    context.fill(Path(roundedRect: track, cornerRadius: 2), with: .color(.secondary.opacity(0.35)))
                    let done = CGRect(x: inset, y: trackY - 2, width: max(0, x(value) - inset), height: 4)
                    context.fill(Path(roundedRect: done, cornerRadius: 2), with: .color(.accentColor))
                    var ticks = Path()
                    for i in stride(from: 0, through: count - 1, by: minor) {
                        ticks.addRect(CGRect(x: x(i) - 0.5, y: trackY + 8, width: 1, height: 3))
                    }
                    for i in labels {
                        ticks.addRect(CGRect(x: x(i) - 0.5, y: trackY + 8, width: 1, height: 6))
                    }
                    context.fill(ticks, with: .color(.secondary))
                }

                ForEach(labels, id: \.self) { i in
                    if abs(x(i) - x(value)) > 18 {
                        Text("\(i)")
                            .font(.caption2.monospacedDigit())
                            .foregroundStyle(.secondary)
                            .fixedSize()
                            .position(x: x(i), y: trackY + 24)
                    }
                }

                // The current channel, in a tag under the thumb (like CARTA).
                Text("\(value)")
                    .font(.caption.weight(.semibold).monospacedDigit())
                    .fixedSize()
                    .padding(.horizontal, 5)
                    .padding(.vertical, 1)
                    .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 4, style: .continuous))
                    .position(x: x(value), y: trackY + 24)

                Circle()
                    .fill(.white)
                    .shadow(color: .black.opacity(0.3), radius: 2, y: 1)
                    .frame(width: thumb, height: thumb)
                    .scaleEffect(isDragging ? 1.15 : 1)
                    .animation(.snappy(duration: 0.15), value: isDragging)
                    .position(x: x(value), y: trackY)
            }
            .contentShape(Rectangle())
            .gesture(
                DragGesture(minimumDistance: 0)
                    .onChanged { g in
                        isDragging = true
                        let i = Int(((g.location.x - inset) / usable * last).rounded())
                        let v = min(max(i, 0), count - 1)
                        if v != value { value = v }
                    }
                    .onEnded { _ in isDragging = false }
            )
        }
        .frame(height: 48)
        // A click for each channel while dragging (not during playback).
        .sensoryFeedback(.selection, trigger: value) { _, _ in isDragging }
        .accessibilityElement()
        .accessibilityLabel(label)
        .accessibilityValue("\(value) of \(max(count - 1, 0))")
        .accessibilityAdjustableAction { direction in
            switch direction {
            case .increment: value = min(value + 1, count - 1)
            case .decrement: value = max(value - 1, 0)
            @unknown default: break
            }
        }
    }
}

/// Two-thumb slider for the playback range (whole channels).
struct ChannelRangeSlider: View {
    @Binding var lower: Int
    @Binding var upper: Int
    let count: Int

    @State private var activeThumb: Int? = nil      // 0 = lower, 1 = upper
    private let thumbWidth: CGFloat = 14
    private let trackY: CGFloat = 12

    var body: some View {
        GeometryReader { geo in
            let inset = thumbWidth / 2 + 4
            let usable = max(1, geo.size.width - 2 * inset)
            let last = CGFloat(max(count - 1, 1))
            let x: (Int) -> CGFloat = { inset + usable * CGFloat($0) / last }
            let close = x(upper) - x(lower) < 30

            ZStack(alignment: .topLeading) {
                Capsule()
                    .fill(.secondary.opacity(0.35))
                    .frame(width: usable, height: 4)
                    .position(x: inset + usable / 2, y: trackY)
                Capsule()
                    .fill(Color.accentColor)
                    .frame(width: max(0, x(upper) - x(lower)), height: 4)
                    .position(x: (x(lower) + x(upper)) / 2, y: trackY)

                ForEach([lower, upper].indices, id: \.self) { t in
                    let v = t == 0 ? lower : upper
                    RoundedRectangle(cornerRadius: 4, style: .continuous)
                        .fill(.white)
                        .shadow(color: .black.opacity(0.3), radius: 1.5, y: 1)
                        .frame(width: thumbWidth, height: 22)
                        .scaleEffect(activeThumb == t ? 1.12 : 1)
                        .position(x: x(v), y: trackY)
                }

                if close {
                    rangeTag(lower == upper ? "\(lower)" : "\(lower)–\(upper)")
                        .position(x: (x(lower) + x(upper)) / 2, y: trackY + 24)
                } else {
                    rangeTag("\(lower)").position(x: x(lower), y: trackY + 24)
                    rangeTag("\(upper)").position(x: x(upper), y: trackY + 24)
                }
            }
            .contentShape(Rectangle())
            .gesture(
                DragGesture(minimumDistance: 0)
                    .onChanged { g in
                        let i = min(max(Int(((g.location.x - inset) / usable * last).rounded()), 0), count - 1)
                        if activeThumb == nil {
                            let sx = g.startLocation.x
                            if lower == upper {
                                activeThumb = (upper == count - 1 || sx < x(lower)) ? 0 : 1
                            } else {
                                activeThumb = abs(sx - x(lower)) <= abs(sx - x(upper)) ? 0 : 1
                            }
                        }
                        if activeThumb == 0 {
                            let v = min(i, upper)
                            if v != lower { lower = v }
                        } else {
                            let v = max(i, lower)
                            if v != upper { upper = v }
                        }
                    }
                    .onEnded { _ in activeThumb = nil }
            )
        }
        .frame(height: 48)
        .sensoryFeedback(.selection, trigger: lower)
        .sensoryFeedback(.selection, trigger: upper)
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("Playback range")
        .accessibilityValue("\(lower) to \(upper)")
    }

    private func rangeTag(_ text: String) -> some View {
        Text(text)
            .font(.caption.weight(.semibold).monospacedDigit())
            .fixedSize()
            .padding(.horizontal, 5)
            .padding(.vertical, 1)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 4, style: .continuous))
    }
}
