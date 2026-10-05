//
//  SpectraViews.swift
//  iFITS Start
//
//  The Spectra dock (bottom, in Spectra mode) and the spectrum box (top right, in other modes).
//

import SwiftUI

/// Everything the spectrum views show, worked out by ContentView.
struct SpectrumDisplay {
    /// The spectral axis.
    let axis: CubeAxis
    /// What the spectrum is taken over (a deleted region has fallen back to Active).
    let source: SpectrumSource
    /// Per-channel values for the chosen statistic, or nil when there's nothing to show yet.
    let values: [Double]?
    /// Pixels measured in each channel.
    let counts: [Int]?
    /// Channel shown in the image.
    let current: Int
    /// "Active pixel (x 120, y 88)", "Entire Image", "Region 2".
    let sourceName: String
    /// One pixel (Active or a point region): no statistic to choose.
    let isSinglePixel: Bool
    let statistic: SpectrumStatistic
    /// BUNIT.
    let unit: String
    /// Why there's no spectrum (shown in the graph).
    let placeholder: String?

    var statisticTitle: String { isSinglePixel ? "Value" : statistic.title }

    var yTitle: String { unit.isEmpty ? statisticTitle : "\(statisticTitle) (\(unit))" }

    /// "Channel 42 · 230.5380 GHz · Mean 1.234e-3 Jy/beam · 25 px".
    func readout(compact: Bool) -> String {
        var parts = [compact ? "Ch \(current)" : "\(axis.name) \(current)", axis.summary(at: current)]
        if let values, values.indices.contains(current) {
            let v = values[current]
            let text = v.isFinite ? String(format: "%.5g", v) : "NaN"
            parts.append((compact ? "" : statisticTitle + " ") + text + (unit.isEmpty ? "" : " " + unit))
            if !compact, !isSinglePixel, let counts, counts.indices.contains(current) {
                parts.append("\(counts[current]) px")
            }
        }
        return parts.filter { !$0.isEmpty }.joined(separator: " · ")
    }
}

/// Menu of what the spectrum is taken over: the Active pixel, the entire image, or a region
/// (ellipses, rectangles and points; lines have no area, so they aren't listed).
struct SpectrumSourceMenu: View {
    @Bindable var model: SpectrumModel
    let regions: [FITSRegion]
    /// The source in use (checked in the menu).
    let selected: SpectrumSource
    let currentName: String

    var body: some View {
        Menu {
            choice(.active, "Active Pixel (Pixel Info)", systemImage: "scope")
            choice(.entireImage, "Entire Image", systemImage: "photo")
            if !regions.isEmpty {
                Divider()
                ForEach(regions) { region in
                    choice(.region(region.id), region.name, systemImage: region.shape.symbol)
                }
            }
        } label: {
            DockMenuLabel(text: currentName)
        }
        .menuOrder(.fixed)
        .accessibilityLabel("Spectrum of \(currentName)")
    }

    private func choice(_ source: SpectrumSource, _ title: String, systemImage: String) -> some View {
        Button {
            model.source = source
        } label: {
            if selected == source {
                Label(title, systemImage: "checkmark")
            } else {
                Label(title, systemImage: systemImage)
            }
        }
    }
}

/// Sum / Mean / StdDev / Min / Max / RMS. Grayed out for a single pixel.
struct SpectrumStatisticMenu: View {
    @Bindable var model: SpectrumModel
    let isSinglePixel: Bool

    var body: some View {
        Menu {
            Picker("Statistic", selection: $model.statistic) {
                ForEach(SpectrumStatistic.allCases) { s in
                    Text(s.title).tag(s)
                }
            }
        } label: {
            DockMenuLabel(text: model.statistic.title)
                .frame(minWidth: 120)
        }
        .menuOrder(.fixed)
        .disabled(isSinglePixel)
        .opacity(isSinglePixel ? 0.4 : 1)
        .accessibilityLabel(isSinglePixel ? "Statistic (not used for a single pixel)" : "Statistic: \(model.statistic.title)")
    }
}

/// Spectra mode: the bottom dock with the source and statistic menus and the spectrum.
struct SpectraDockPanel: View {
    @Bindable var model: SpectrumModel
    let display: SpectrumDisplay
    let regions: [FITSRegion]
    let glassNamespace: Namespace.ID
    var onChannel: (Int) -> Void

    var body: some View {
        CollapsibleDock(expanded: $model.expanded, glassNamespace: glassNamespace) {
            VStack(alignment: .leading, spacing: 10) {
                HStack(spacing: 12) {
                    Label("Spectra", systemImage: "chart.xyaxis.line")
                        .font(.headline)
                        .lineLimit(1)
                        .fixedSize()
                    Spacer(minLength: 0)
                    Text("Region")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                    SpectrumSourceMenu(model: model, regions: regions, selected: display.source,
                                       currentName: display.sourceName)
                    Text("Statistic")
                        .font(.subheadline)
                        .foregroundStyle(display.isSinglePixel ? .tertiary : .secondary)
                    SpectrumStatisticMenu(model: model, isSinglePixel: display.isSinglePixel)
                    DockCollapseButton(expanded: $model.expanded)
                }
                Divider()
                HStack(spacing: 10) {
                    Text(display.readout(compact: false))
                        .font(.callout.monospacedDigit())
                        .lineLimit(1)
                    Spacer(minLength: 8)
                    if model.isComputing, model.showsProgress {
                        ProgressView(value: model.progress)
                            .frame(width: 120)
                    } else if model.zoom != nil {
                        Button {
                            model.zoom = nil
                        } label: {
                            Label("Zoom Out", systemImage: "arrow.up.left.and.arrow.down.right")
                                .font(.subheadline)
                        }
                        .buttonStyle(.plain)
                        .hoverEffect(.highlight)
                    } else {
                        Text("Drag the orange line · Pinch to zoom")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                }
                SpectrumGraph(values: display.values, axis: display.axis, yTitle: display.yTitle,
                              current: display.current, zoom: $model.zoom,
                              placeholder: display.placeholder, onChannel: onChannel)
                    .frame(height: 230)
            }
        } mini: {
            HStack(spacing: 10) {
                Image(systemName: "chart.xyaxis.line")
                Text(display.sourceName)
                    .font(.subheadline.weight(.semibold))
                    .lineLimit(1)
                Text(display.statisticTitle)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                Image(systemName: "chevron.up")
                    .font(.caption.weight(.bold))
            }
        }
    }
}

/// Top-right glass box with the spectrum, shown in every mode but Spectra after you leave Spectra
/// mode, until closed with ✕ (like the statistics box).
struct SpectrumBox: View {
    @Bindable var model: SpectrumModel
    let display: SpectrumDisplay
    var onChannel: (Int) -> Void
    var onClose: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 6) {
                Image(systemName: "chart.xyaxis.line")
                    .foregroundStyle(.secondary)
                Text("Spectrum")
                    .font(.subheadline.weight(.semibold))
                Text("\(display.sourceName) · \(display.statisticTitle)")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                Spacer(minLength: 12)
                if model.isComputing, model.showsProgress {
                    ProgressView()
                        .controlSize(.small)
                }
                if model.zoom != nil {
                    smallButton("arrow.up.left.and.arrow.down.right", label: "Zoom out") { model.zoom = nil }
                }
                smallButton("xmark", label: "Close spectrum", action: onClose)
            }
            SpectrumGraph(values: display.values, axis: display.axis, yTitle: display.yTitle,
                          current: display.current, zoom: $model.zoom, compact: true,
                          placeholder: display.placeholder, onChannel: onChannel)
                .frame(width: 300, height: 130)
            Text(display.readout(compact: true))
                .font(.caption.monospacedDigit())
                .foregroundStyle(.secondary)
                .lineLimit(1)
        }
        .padding(14)
        .frame(width: 328)
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 20, style: .continuous))
    }

    private func smallButton(_ symbol: String, label: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: symbol)
                .font(.caption.weight(.bold))
                .foregroundStyle(.secondary)
                .frame(width: 26, height: 26)
                .background(.quaternary, in: Circle())
                .contentShape(Circle())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityLabel(label)
    }
}
