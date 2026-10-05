//
//  ImageColorbar.swift
//  iFITS Start
//
//  Collapsible colorbar (bottom right of the main view). It follows the Render Configuration:
//  colormap, inversion, scaling and clip range.
//

import SwiftUI

struct ImageColorbar: View {
    let settings: RenderSettings
    let unit: String
    @Binding var expanded: Bool

    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    var body: some View {
        Group {
            if expanded {
                full
                    .padding(.horizontal, 14)
                    .padding(.top, 10)
                    .padding(.bottom, 12)
                    .frame(width: 320)
                    .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 18, style: .continuous))
            } else {
                Button {
                    withAnimation(DockAnimation.stage(reduceMotion)) { expanded = true }
                } label: {
                    HStack(spacing: 8) {
                        swatch
                            .frame(width: 56, height: 10)
                            .clipShape(Capsule())
                        Image(systemName: "chevron.up")
                            .font(.caption.weight(.bold))
                    }
                    .padding(.horizontal, 14)
                    .padding(.vertical, 10)
                    .contentShape(Capsule())
                }
                .buttonStyle(.plain)
                .glassEffect(.regular.interactive(), in: Capsule())
                .hoverEffect(.lift)
                .accessibilityLabel("Show colorbar")
            }
        }
    }

    private var full: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(spacing: 6) {
                Text(unit.isEmpty ? "Value" : "Value (\(unit))")
                    .font(.caption.weight(.semibold))
                Text("· \(settings.colormap.title)\(settings.inverted ? ", inverted" : "") · \(settings.scaling.title)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                Spacer(minLength: 4)
                Button {
                    withAnimation(DockAnimation.stage(reduceMotion)) { expanded = false }
                } label: {
                    Image(systemName: "chevron.down")
                        .font(.caption.weight(.bold))
                        .frame(width: 26, height: 22)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .hoverEffect(.highlight)
                .accessibilityLabel("Hide colorbar")
            }
            swatch
                .frame(height: 14)
                .clipShape(RoundedRectangle(cornerRadius: 3, style: .continuous))
                .overlay(RoundedRectangle(cornerRadius: 3, style: .continuous).stroke(.white.opacity(0.3), lineWidth: 0.5))
            ticks
                .frame(height: 18)
        }
    }

    private var swatch: some View {
        Group {
            if let image = settings.colormap.swatchImage(inverted: settings.inverted) {
                Image(uiImage: image)
                    .resizable()
            } else {
                Color.gray
            }
        }
    }

    /// Round values along the bar. With a non-linear scaling they sit where that value's colour is.
    private var ticks: some View {
        GeometryReader { geo in
            let lo = settings.clipMin, hi = settings.clipMax
            let (values, step) = NiceTicks.values(lo: lo, hi: hi, target: 4)
            let placed = values.map { v -> (Double, CGFloat) in
                let x = hi > lo ? (v - lo) / (hi - lo) : 0
                let y = settings.scaling.apply(min(max(x, 0), 1), alpha: settings.alpha, gamma: settings.gamma)
                return (v, CGFloat(y.isFinite ? y : 0) * geo.size.width)
            }
            // Skip labels that would overlap the one before.
            let shown = placed.reduce(into: [(Double, CGFloat)]()) { kept, item in
                if let last = kept.last, item.1 - last.1 < 44 { return }
                kept.append(item)
            }
            ForEach(Array(shown.enumerated()), id: \.offset) { _, item in
                VStack(spacing: 1) {
                    Rectangle()
                        .fill(.primary)
                        .frame(width: 1, height: 4)
                    Text(NiceTicks.label(item.0, step: step))
                        .font(.caption2.monospacedDigit())
                        .lineLimit(1)
                        .fixedSize()
                }
                .position(x: min(max(item.1, 12), geo.size.width - 12), y: 9)
            }
        }
    }
}
