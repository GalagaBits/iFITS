//
//  SpectraViews.swift
//  iFITS Start
//
//  The Spectra dock (bottom, in Spectra mode), the spectrum box (top right, in other modes), and
//  the pieces they share with the Spectra window: menus, legend, readout.
//

import SwiftUI

/// One spectrum on the graph (one menu choice).
struct SpectrumSeries: Identifiable {
    let source: SpectrumSource
    /// "Active (x 120, y 88)", "Entire Image", "Region 2".
    let name: String
    let color: Color
    /// Per-channel values for the chosen statistic, or nil while there's nothing to show yet.
    let values: [Double]?
    /// Pixels measured in each channel.
    let counts: [Int]?
    /// One pixel (Active or a point region): its own value, no statistic.
    let isSinglePixel: Bool
    /// The result shown (changes when a new spectrum arrives).
    let resultID: UUID?

    var id: SpectrumSource { source }
}

/// Everything the spectrum views show, worked out by ContentView.
struct SpectrumDisplay {
    /// The spectral axis.
    let axis: CubeAxis
    /// Up to 10 spectra, in colour order.
    let series: [SpectrumSeries]
    /// Channel shown in the image.
    let current: Int
    let statistic: SpectrumStatistic
    /// BUNIT.
    let unit: String
    /// Why there's no spectrum (shown in the graph).
    let placeholder: String?

    /// Every spectrum is a single pixel: no statistic to choose.
    var isSinglePixel: Bool { series.allSatisfy(\.isSinglePixel) }

    var statisticTitle: String { isSinglePixel ? "Value" : statistic.title }

    var yTitle: String { unit.isEmpty ? statisticTitle : "\(statisticTitle) (\(unit))" }

    /// The selection in the menu: the one name, or "3 spectra".
    var sourceName: String {
        series.count == 1 ? series[0].name : "\(series.count) spectra"
    }

    var sources: [SpectrumSource] { series.map(\.source) }

    /// The lines to draw.
    var lines: [SpectrumLine] {
        series.compactMap { s in s.values.map { SpectrumLine(values: $0, color: s.color) } }
    }

    /// A value at a channel, as text ("1555.2 MJy/sr").
    func valueText(_ s: SpectrumSeries, channel: Int, withUnit: Bool = true) -> String {
        guard let values = s.values, values.indices.contains(channel) else { return "—" }
        let v = values[channel]
        let text = v.isFinite ? String(format: "%.5g", v) : "NaN"
        return text + (withUnit && !unit.isEmpty ? " " + unit : "")
    }

    /// "Channel 42 · 230.5380 GHz · Mean 1.234e-3 Jy/beam · 25 px". With several spectra, only the
    /// channel (the legend has the values).
    func readout(compact: Bool, channel: Int? = nil) -> String {
        let current = channel ?? self.current
        var parts = [compact ? "Ch \(current)" : "\(axis.name) \(current)", axis.summary(at: current)]
        if series.count == 1, let s = series.first, s.values != nil {
            parts.append((compact ? "" : (s.isSinglePixel ? "Value" : statistic.title) + " ")
                         + valueText(s, channel: current))
            if !compact, !s.isSinglePixel, let counts = s.counts, counts.indices.contains(current) {
                parts.append("\(counts[current]) px")
            }
        }
        return parts.filter { !$0.isEmpty }.joined(separator: " · ")
    }
}

/// Colour key for several spectra: a line in each colour, its name, and (unless compact) its value
/// at the current channel. Scrolls sideways if it doesn't fit.
struct SpectrumLegend: View {
    let display: SpectrumDisplay
    var channel: Int? = nil
    var compact = false

    var body: some View {
        ScrollView(.horizontal) {
            HStack(spacing: compact ? 10 : 16) {
                ForEach(display.series) { s in
                    HStack(spacing: 5) {
                        Capsule()
                            .fill(s.color)
                            .frame(width: compact ? 12 : 16, height: 3)
                        Text(s.name)
                            .foregroundStyle(.primary)
                        if !compact {
                            Text(display.valueText(s, channel: channel ?? display.current))
                                .foregroundStyle(.secondary)
                                .monospacedDigit()
                        }
                    }
                    .font(compact ? .caption2 : .caption)
                    .lineLimit(1)
                }
            }
        }
        .scrollIndicators(.hidden)
    }
}

/// Menu of what the spectra are taken over: the Active pixel, the entire image, or regions
/// (ellipses, rectangles and points; lines have no area, so they aren't listed). Pick several to
/// overplot them (up to 10); the menu stays open while you pick.
struct SpectrumSourceMenu: View {
    @Bindable var model: SpectrumModel
    let regions: [FITSRegion]
    /// The sources in use, in colour order (deleted regions already dropped).
    let selected: [SpectrumSource]
    let currentName: String

    var body: some View {
        Menu {
            Section("Overplot up to \(SpectrumModel.maxSources)") {
                choice(.active, "Active Pixel (Pixel Info)", systemImage: "scope")
                choice(.entireImage, "Entire Image", systemImage: "photo")
            }
            if !regions.isEmpty {
                Section("Regions") {
                    ForEach(regions) { region in
                        choice(.region(region.id), region.name, systemImage: region.shape.symbol)
                    }
                }
            }
            if selected.count > 1 {
                Divider()
                Button {
                    model.selectOnly(selected[0])
                } label: {
                    Label("Show Only the First", systemImage: "line.diagonal")
                }
            }
        } label: {
            DockMenuLabel(text: currentName)
        }
        .menuOrder(.fixed)
        .menuActionDismissBehavior(.disabled)
        .accessibilityLabel("Spectra of \(currentName)")
    }

    private func choice(_ source: SpectrumSource, _ title: String, systemImage: String) -> some View {
        let index = selected.firstIndex(of: source)
        let full = selected.count >= SpectrumModel.maxSources
        return Button {
            model.toggle(source)
        } label: {
            if let index {
                // Position in the colour cycle: 1 = blue, 2 = orange, …
                Label("\(title)  (\(index + 1))", systemImage: "checkmark")
            } else {
                Label(title, systemImage: systemImage)
            }
        }
        .disabled(index == nil && full)
    }
}

/// Sum / Mean / StdDev / Min / Max / RMS. Grayed out when every spectrum is a single pixel.
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

/// Spectra mode: the bottom dock with the source and statistic menus and the spectra.
struct SpectraDockPanel: View {
    @Bindable var model: SpectrumModel
    let display: SpectrumDisplay
    let regions: [FITSRegion]
    let glassNamespace: Namespace.ID
    var onChannel: (Int) -> Void
    /// Opens the spectra in their own window.
    var onPopOut: () -> Void

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
                    SpectrumSourceMenu(model: model, regions: regions, selected: display.sources,
                                       currentName: display.sourceName)
                    Text("Statistic")
                        .font(.subheadline)
                        .foregroundStyle(display.isSinglePixel ? .tertiary : .secondary)
                    SpectrumStatisticMenu(model: model, isSinglePixel: display.isSinglePixel)
                    Button(action: onPopOut) {
                        Image(systemName: "macwindow.badge.plus")
                            .font(.body.weight(.semibold))
                            .frame(width: 36, height: 36)
                            .contentShape(Rectangle())
                    }
                    .buttonStyle(.plain)
                    .hoverEffect(.highlight)
                    .accessibilityLabel("Open spectra in a new window")
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
                        Text("Drag the orange line · Pinch or two fingers up / down to zoom")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                }
                SpectrumGraph(lines: display.lines, axis: display.axis, yTitle: display.yTitle,
                              current: display.current, zoom: $model.zoom,
                              placeholder: display.placeholder, onChannel: onChannel)
                    .frame(height: 230)
                if display.series.count > 1 {
                    SpectrumLegend(display: display)
                }
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

/// Top-right glass box with the spectra, shown in every mode but Spectra after you leave Spectra
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
            SpectrumGraph(lines: display.lines, axis: display.axis, yTitle: display.yTitle,
                          current: display.current, zoom: $model.zoom, compact: true,
                          placeholder: display.placeholder, onChannel: onChannel)
                .frame(width: 300, height: 130)
            if display.series.count > 1 {
                SpectrumLegend(display: display, compact: true)
                    .frame(width: 300)
            }
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
