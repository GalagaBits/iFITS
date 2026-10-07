//
//  SpectrumExport.swift
//  iFITS Start
//
//  The spectra as a picture for "Export and Send": white background, "<file> — Spectra" on top,
//  axis titles, the legend on the right. A fixed 9 × 5 inch figure at 300 dpi (2700 × 1500 px).
//

import SwiftUI
import UIKit

enum SpectrumExport {
    /// Figure size in points, and pixels per point (300 dpi at 100 points per inch).
    static let size = CGSize(width: 900, height: 500)
    static let scale: CGFloat = 3

    /// The figure as shown (same spectra, statistic and zoom).
    @MainActor
    static func render(display: SpectrumDisplay, fileName: String, zoom: ClosedRange<Double>?) -> UIImage? {
        let figure = SpectrumFigure(display: display,
                                    title: fileName.isEmpty ? "Spectra" : "\(fileName) — Spectra",
                                    zoom: zoom)
            .frame(width: size.width, height: size.height)
            .environment(\.colorScheme, .light)
        let renderer = ImageRenderer(content: figure)
        renderer.scale = scale
        renderer.isOpaque = true
        return renderer.uiImage
    }

    /// "ngc2403 spectra".
    static func baseName(fileName: String) -> String {
        let base = (fileName as NSString).deletingPathExtension
        return base.isEmpty ? "Spectra" : "\(base) spectra"
    }
}

/// The exported figure.
private struct SpectrumFigure: View {
    let display: SpectrumDisplay
    let title: String
    let zoom: ClosedRange<Double>?

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(title)
                .font(.system(size: 17, weight: .semibold))
                .frame(maxWidth: .infinity)
                .lineLimit(1)
                .minimumScaleFactor(0.6)
            HStack(alignment: .center, spacing: 18) {
                SpectrumGraph(lines: display.lines, axis: display.axis, yTitle: display.yTitle,
                              current: display.current, zoom: .constant(zoom),
                              placeholder: display.placeholder, interactive: false)
                legend
                    .frame(width: 170, alignment: .leading)
            }
        }
        .padding(.horizontal, 24)
        .padding(.vertical, 18)
        .foregroundStyle(.black)
        .background(Color.white)
    }

    /// One row per spectrum: its line colour and name.
    private var legend: some View {
        VStack(alignment: .leading, spacing: 8) {
            ForEach(display.series) { s in
                HStack(spacing: 8) {
                    Capsule()
                        .fill(s.color)
                        .frame(width: 22, height: 3)
                    Text(s.name)
                        .font(.system(size: 11))
                        .lineLimit(2)
                }
            }
            if !display.isSinglePixel {
                Text("Statistic: \(display.statistic.title)")
                    .font(.system(size: 10))
                    .foregroundStyle(.gray)
                    .padding(.top, 4)
            }
        }
        .padding(10)
        .overlay(RoundedRectangle(cornerRadius: 4).stroke(Color.gray.opacity(0.5), lineWidth: 0.5))
    }
}
