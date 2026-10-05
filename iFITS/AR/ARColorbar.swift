//
//  ARColorbar.swift
//  iFITS Start
//
//  The AR view's colorbar: colour and transparency for each value (low values clear, high values
//  solid), with tick values in the image's units. Tap it to choose another colormap.
//

import SwiftUI

struct ARColorbar: View {
    let colormap: Colormap
    let inverted: Bool
    let lo: Double
    let hi: Double
    let unit: String
    var onSelect: (Colormap) -> Void
    var onToggleInverted: () -> Void

    private let barWidth: CGFloat = 22

    var body: some View {
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
        }
        .menuOrder(.fixed)
        .accessibilityLabel("Colorbar, \(colormap.title). Tap to change the colormap.")
    }

    private var bar: some View {
        HStack(alignment: .center, spacing: 6) {
            Text("Value (\(unit))")
                .font(.caption.weight(.semibold))
                .fixedSize()
                .rotationEffect(.degrees(-90))
                .frame(width: 18)
            gradient
                .frame(width: barWidth)
            ticks
                .frame(width: 66)
        }
        .padding(.vertical, 14)
        .padding(.horizontal, 10)
        .frame(height: 340)
        .glassEffect(.regular.interactive(), in: RoundedRectangle(cornerRadius: 20, style: .continuous))
        .contentShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
    }

    /// Colour over a checkerboard, with opacity rising like the cube's (value² from bottom to top).
    private var gradient: some View {
        let lut = colormap.lut(inverted: inverted)
        return Canvas { context, size in
            // Checkerboard, so the transparency shows.
            let cell: CGFloat = 5.5
            for row in 0..<Int(ceil(size.height / cell)) {
                for col in 0..<Int(ceil(size.width / cell)) {
                    let rect = CGRect(x: CGFloat(col) * cell, y: CGFloat(row) * cell, width: cell, height: cell)
                    context.fill(Path(rect), with: .color((row + col).isMultiple(of: 2) ? Color(white: 0.75) : Color(white: 0.45)))
                }
            }
            let steps = 128
            let h = size.height / CGFloat(steps)
            for i in 0..<steps {
                let s = (Double(i) + 0.5) / Double(steps)        // 0 at the bottom, 1 at the top
                let rgba = lut[min(lut.count - 1, Int(s * Double(lut.count)))]
                let color = Color(red: Double(rgba & 0xFF) / 255,
                                  green: Double((rgba >> 8) & 0xFF) / 255,
                                  blue: Double((rgba >> 16) & 0xFF) / 255,
                                  opacity: min(1, 0.08 + s * s))
                let y = size.height - CGFloat(i + 1) * h
                context.fill(Path(CGRect(x: 0, y: y, width: size.width, height: h + 0.5)), with: .color(color))
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 4, style: .continuous))
        .overlay(RoundedRectangle(cornerRadius: 4, style: .continuous).stroke(.white.opacity(0.35), lineWidth: 0.5))
    }

    private var ticks: some View {
        GeometryReader { geo in
            let (values, step) = NiceTicks.values(lo: lo, hi: hi, target: 5)
            ForEach(Array(values.enumerated()), id: \.offset) { _, v in
                let f = (v - lo) / (hi - lo)
                HStack(spacing: 3) {
                    Rectangle()
                        .fill(.primary)
                        .frame(width: 5, height: 1)
                    Text(NiceTicks.label(v, step: step))
                        .font(.caption2.monospacedDigit())
                        .lineLimit(1)
                        .fixedSize()
                }
                .frame(width: geo.size.width, alignment: .leading)
                .position(x: geo.size.width / 2, y: geo.size.height * CGFloat(1 - f))
            }
        }
    }
}
