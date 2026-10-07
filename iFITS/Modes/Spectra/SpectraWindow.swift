//
//  SpectraWindow.swift
//  iFITS Start
//
//  Spectra in their own iPadOS window ("<file> — Spectra"), opened from the Spectra dock. The
//  window and the main window share the same spectrum: changing the region, statistic, zoom or
//  channel in one changes the other.
//

import SwiftUI

/// Connects a main window to its Spectra window. The main window keeps `display` up to date; the
/// Spectra window shows it and sends changes back (the menus change the shared SpectrumModel, the
/// orange line calls `onChannel`).
@MainActor
@Observable
final class SpectraWindowLink {
    let id = UUID()
    /// What to show (nil = no cube open).
    var display: SpectrumDisplay?
    /// Regions that have a spectrum (ellipses, rectangles, points).
    var regions: [FITSRegion] = []
    var fileName = ""
    /// The main window's spectrum settings and result (shared, so both windows stay in step).
    var model: SpectrumModel?
    /// The channel on screen. Set straight away when the line is dragged in the Spectra window, so it
    /// moves even before the main window has redrawn.
    var current: Int?
    @ObservationIgnored var onChannel: (Int) -> Void = { _ in }
    /// Spectra windows showing this link (the main window keeps computing spectra while any is open).
    private(set) var openWindows = 0

    var isOpen: Bool { openWindows > 0 }

    func windowAppeared() { openWindows += 1 }
    func windowDisappeared() { openWindows = max(0, openWindows - 1) }

    /// Every main window's link, so a Spectra window (opened with the link's id) can find it.
    private static var links: [UUID: SpectraWindowLink] = [:]

    static func register(_ link: SpectraWindowLink) { links[link.id] = link }
    static func link(_ id: UUID?) -> SpectraWindowLink? { id.flatMap { links[$0] } }
}

/// The Spectra window (or a sheet, where extra windows aren't available).
struct SpectraWindowView: View {
    let link: SpectraWindowLink?
    /// Set when shown as a sheet instead of a window.
    var onClose: (() -> Void)? = nil

    @Environment(\.dismissWindow) private var dismissWindow

    private var title: String {
        guard let name = link?.fileName, !name.isEmpty else { return "Spectra" }
        return "\(name) — Spectra"
    }

    var body: some View {
        NavigationStack {
            Group {
                if let link, let model = link.model, let display = link.display {
                    SpectraWindowContent(link: link, model: model, display: display)
                } else {
                    ContentUnavailableView("No Spectrum",
                                           systemImage: "chart.xyaxis.line",
                                           description: Text("Open a cube in iFITS, then use the pop-out button in Spectra mode."))
                }
            }
            .navigationTitle(title)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarLeading) {
                    Button { close() } label: {
                        Image(systemName: "xmark")
                    }
                    .keyboardShortcut(.cancelAction)
                    .accessibilityLabel("Close")
                }
            }
        }
        .onAppear { link?.windowAppeared() }
        .onDisappear { link?.windowDisappeared() }
    }

    private func close() {
        if let onClose {
            onClose()
        } else {
            dismissWindow()
        }
    }
}

/// A big spectrum filling the window from the top, with the legend, readout, menus and Zoom Out
/// underneath.
private struct SpectraWindowContent: View {
    let link: SpectraWindowLink
    @Bindable var model: SpectrumModel
    let display: SpectrumDisplay

    /// The "Export and Send" sheet for this window.
    @State private var exportRequest: ExportRequest?

    private var current: Int { link.current ?? display.current }

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            // Takes every point of height the controls don't need.
            SpectrumGraph(lines: display.lines, axis: display.axis, yTitle: display.yTitle,
                          current: current, zoom: $model.zoom,
                          placeholder: display.placeholder, onChannel: { link.onChannel($0) })
                .frame(maxWidth: .infinity, minHeight: 120, maxHeight: .infinity)
                .layoutPriority(1)
            if display.series.count > 1 {
                SpectrumLegend(display: display, channel: current)
            }
            HStack(spacing: 10) {
                Text(display.readout(compact: false, channel: current))
                    .font(.callout.monospacedDigit())
                    .lineLimit(1)
                    .minimumScaleFactor(0.8)
                Spacer(minLength: 8)
                if model.isComputing, model.showsProgress {
                    ProgressView(value: model.progress)
                        .frame(width: 120)
                }
            }
            ViewThatFits(in: .horizontal) {
                HStack(spacing: 12) { controls }
                VStack(alignment: .leading, spacing: 8) { controls }
            }
        }
        .padding(.horizontal, 20)
        .padding(.top, 8)
        .padding(.bottom, 16)
        .sheet(item: $exportRequest) { request in
            ExportSheet(request: request, onClose: { exportRequest = nil })
                .presentationSizing(.form)
        }
    }

    /// "Export and Send" the spectra as shown here.
    private func export() {
        let name = link.fileName
        let zoom = model.zoom
        let shown = display
        exportRequest = ExportRequest(title: "Export Spectra", baseName: SpectrumExport.baseName(fileName: name)) {
            SpectrumExport.render(display: shown, fileName: name, zoom: zoom)
        }
    }

    @ViewBuilder
    private var controls: some View {
        HStack(spacing: 8) {
            Text("Region")
                .font(.subheadline)
                .foregroundStyle(.secondary)
            SpectrumSourceMenu(model: model, regions: link.regions, selected: display.sources,
                               currentName: display.sourceName)
        }
        HStack(spacing: 8) {
            Text("Statistic")
                .font(.subheadline)
                .foregroundStyle(display.isSinglePixel ? .tertiary : .secondary)
            SpectrumStatisticMenu(model: model, isSinglePixel: display.isSinglePixel)
        }
        Spacer(minLength: 0)
        Button {
            model.zoom = nil
        } label: {
            Label("Zoom Out", systemImage: "arrow.up.left.and.arrow.down.right")
                .font(.subheadline)
        }
        .disabled(model.zoom == nil)
        Button {
            export()
        } label: {
            Label("Export and Send", systemImage: "square.and.arrow.up")
                .font(.subheadline)
        }
        .disabled(display.lines.isEmpty)
    }
}
