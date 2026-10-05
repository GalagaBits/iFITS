//
//  VerticalColorbar.swift
//  iFITS Start
//
//  The vertical, collapsible colorbar used at the bottom left of both the main view and the AR
//  view: "Value (unit)" up the side, the colour bar, and tick values. Tap the bar to choose another
//  colormap (or invert it); the chevron folds it into a small pill.
//

import SwiftUI

struct VerticalColorbar: View {
    let colormap: Colormap
    let inverted: Bool
    /// Values at the bottom and top of the bar.
    let lo: Double
    let hi: Double
    let unit: String
    /// Where a value's colour sits on the bar (0 = bottom, 1 = top), from its place between lo and
    /// hi (0…1). Linear unless the image uses another scaling.
    var valuePosition: (Double) -> Double = { $0 }
    /// AR: show transparency too (low values clear, high values solid) over a checkerboard.
    var showsOpacity = false
    /// Height of the bar itself.
    var barHeight: CGFloat = 300
    @Binding var expanded: Bool
    /// Not enough room for the full colorbar: only the pill is shown.
    var pillOnly = false
    var onSelect: (Colormap) -> Void
    var onToggleInverted: () -> Void

    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    static let barWidth: CGFloat = 22
    /// Width of the expanded colorbar (for layout around it).
    static let width: CGFloat = 146

    var body: some View {
        if expanded && !pillOnly {
            full
        } else {
            pill
        }
    }

    private var title: String { unit.isEmpty ? "Value" : "Value (\(unit))" }

    // MARK: Expanded

    private var full: some View {
        VStack(spacing: 4) {
            HStack {
                Spacer()
                Button {
                    withAnimation(DockAnimation.stage(reduceMotion)) { expanded = false }
                } label: {
                    Image(systemName: "chevron.down")
                        .font(.caption.weight(.bold))
                        .frame(width: 30, height: 24)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .hoverEffect(.highlight)
                .accessibilityLabel("Hide colorbar")
            }
            Menu {
                Section("Colormap") {
                    ForEach(Colormap.allCases) { map in
                        Button {
                            onSelect(map)
                        } label: {
                            if map == colormap {
                                Label(map.title, systemImage: "checkmark")
                            } else {
                                Text(map.title)
                            }
                        }
                    }
                }
                Button {
                    onToggleInverted()
                } label: {
                    if inverted {
                        Label("Inverted", systemImage: "checkmark")
                    } else {
                        Text("Inverted")
                    }
                }
            } label: {
                bar
                    // Menu labels are tinted with the accent colour (blue); keep the normal text colour.
                    .foregroundStyle(Color.primary)
            }
            .menuOrder(.fixed)
            .tint(Color.primary)
            .accessibilityLabel("Colorbar, \(colormap.title)\(inverted ? ", inverted" : ""). Tap to change the colormap.")
        }
        .padding(.top, 6)
        .padding(.bottom, 14)
        .padding(.horizontal, 10)
        .frame(width: Self.width)
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 20, style: .continuous))
    }

    private var bar: some View {
        HStack(alignment: .center, spacing: 6) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(Color.primary)
                .lineLimit(1)
                .fixedSize()
                .rotationEffect(.degrees(-90))
                .frame(width: 18)
            gradient
                .frame(width: Self.barWidth)
            ticks
                .frame(maxWidth: .infinity)
        }
        .frame(height: barHeight)
        .contentShape(Rectangle())
    }

    /// The colormap from bottom (low) to top (high). With transparency: over a checkerboard, opacity
    /// rising like the AR cube's (value² from bottom to top).
    private var gradient: some View {
        let lut = colormap.lut(inverted: inverted)
        let opacity = showsOpacity
        return Canvas { context, size in
            if opacity {
                let cell: CGFloat = 5.5
                for row in 0..<Int(ceil(size.height / cell)) {
                    for col in 0..<Int(ceil(size.width / cell)) {
                        let rect = CGRect(x: CGFloat(col) * cell, y: CGFloat(row) * cell, width: cell, height: cell)
                        context.fill(Path(rect), with: .color((row + col).isMultiple(of: 2) ? Color(white: 0.75) : Color(white: 0.45)))
                    }
                }
            }
            let steps = 128
            let h = size.height / CGFloat(steps)
            for i in 0..<steps {
                let s = (Double(i) + 0.5) / Double(steps)        // 0 at the bottom, 1 at the top
                let rgba = lut[min(lut.count - 1, max(0, Int(s * Double(lut.count))))]
                let color = Color(red: Double(rgba & 0xFF) / 255,
                                  green: Double((rgba >> 8) & 0xFF) / 255,
                                  blue: Double((rgba >> 16) & 0xFF) / 255,
                                  opacity: opacity ? min(1, 0.08 + s * s) : 1)
                let y = size.height - CGFloat(i + 1) * h
                context.fill(Path(CGRect(x: 0, y: y, width: size.width, height: h + 0.5)), with: .color(color))
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 4, style: .continuous))
        .overlay(RoundedRectangle(cornerRadius: 4, style: .continuous).stroke(.white.opacity(0.35), lineWidth: 0.5))
    }

    /// Round values beside the bar, each where that value's colour is. Labels that would overlap
    /// the one below are skipped.
    private var ticks: some View {
        GeometryReader { geo in
            let (values, step) = NiceTicks.values(lo: lo, hi: hi, target: max(2, Int(barHeight / 60)))
            let placed = values.compactMap { v -> (Double, CGFloat)? in
                guard hi > lo else { return nil }
                let f = valuePosition(min(max((v - lo) / (hi - lo), 0), 1))
                guard f.isFinite else { return nil }
                return (v, geo.size.height * CGFloat(1 - f))
            }
            let shown = placed.reduce(into: [(Double, CGFloat)]()) { kept, item in
                if let last = kept.last, last.1 - item.1 < 16 { return }
                kept.append(item)
            }
            ForEach(Array(shown.enumerated()), id: \.offset) { _, item in
                HStack(spacing: 3) {
                    Rectangle()
                        .fill(Color.primary)
                        .frame(width: 5, height: 1)
                    Text(NiceTicks.label(item.0, step: step))
                        .font(.caption2.monospacedDigit())
                        .foregroundStyle(Color.primary)
                        .lineLimit(1)
                        .fixedSize()
                }
                .frame(width: geo.size.width, alignment: .leading)
                .position(x: geo.size.width / 2, y: item.1)
            }
        }
    }

    // MARK: Collapsed

    private var pill: some View {
        Button {
            withAnimation(DockAnimation.stage(reduceMotion)) { expanded = true }
        } label: {
            VStack(spacing: 6) {
                Image(systemName: "chevron.up")
                    .font(.caption.weight(.bold))
                gradient
                    .frame(width: 10, height: 44)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 10)
            .contentShape(Capsule())
        }
        .buttonStyle(.plain)
        .glassEffect(.regular.interactive(), in: Capsule())
        .hoverEffect(.lift)
        .disabled(pillOnly)
        .accessibilityLabel(pillOnly ? "Colorbar (no room to open it here)" : "Show colorbar")
    }
}
