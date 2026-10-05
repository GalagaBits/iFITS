//
//  ContentView+Spectra.swift
//  iFITS Start
//
//  Spectra mode ("Z", like CARTA's Z profile): spectra of the Active pixel, the entire image or
//  regions along the cube's spectral axis (up to 10 overplotted).
//

import SwiftUI
import UIKit

extension ContentView {
    /// The Spectra button works for cubes (more than one channel) whose data can be read.
    var canUseSpectra: Bool {
        cubeSource != nil && signalImage != nil && fitsImage != nil && spectralAxisIndex != nil
    }

    /// The axis spectra run along: the first spectral axis with more than one channel, otherwise
    /// the first axis with more than one channel.
    var spectralAxisIndex: Int? {
        guard let cube = cubeSource else { return nil }
        return cube.axes.firstIndex { $0.kind == .spectral && $0.length > 1 }
            ?? cube.axes.firstIndex { $0.length > 1 }
    }

    /// The spectrum button (top right): turns Spectra mode on, or back off to the previous mode.
    func toggleSpectraMode() {
        if selectedMode == "Z" {
            selectMode(modeBeforeCube)
        } else {
            guard canUseSpectra else { return }
            if modes.contains(selectedMode) { modeBeforeCube = selectedMode }
            spectrum.expanded = true
            selectMode("Z")
        }
    }

    /// The spectra in use, in colour order. Deleted regions are dropped; never empty.
    var resolvedSpectrumSources: [SpectrumSource] {
        let valid = spectrum.sources.filter { source in
            if case .region(let id) = source {
                return regionStore.region(id)?.shape.hasStatistics == true
            }
            return true
        }
        return valid.isEmpty ? [.active] : valid
    }

    /// The pixels of a spectrum, or nil (no Active pixel yet, or the region is gone).
    func spectrumArea(for source: SpectrumSource) -> SpectrumArea? {
        switch source {
        case .active:
            guard let pixel = inspectedPixel else { return nil }
            // Pixel Info counts rows from the top; FITS y counts from the bottom.
            return .pixel(x: pixel.column, y: Int(imageHeight) - 1 - pixel.row)
        case .entireImage:
            return .entireImage
        case .region(let id):
            return regionStore.region(id).map { SpectrumArea($0) }
        }
    }

    /// "Active (x 40, y 35)", "Entire Image", "Region 1".
    func spectrumName(for source: SpectrumSource) -> String {
        switch source {
        case .active:
            guard let pixel = inspectedPixel else { return "Active Pixel" }
            return "Active (x \(pixel.column + 1), y \(Int(imageHeight) - pixel.row))"
        case .entireImage:
            return "Entire Image"
        case .region(let id):
            return regionStore.region(id)?.name ?? "Region"
        }
    }

    /// The top-right spectrum box is on screen: not in Spectra mode (which has the dock), and not
    /// while drawing (A mode), when it steps aside until you pick another mode.
    var spectrumBoxVisible: Bool {
        spectrum.showBox && canUseSpectra && selectedMode != "Z" && selectedMode != "A"
    }

    /// What to compute: one key per spectrum (none when no spectrum is on screen). Changing channel
    /// doesn't change them; a region, the Active pixel, or another Stokes (or other axis) channel does.
    var spectrumKeys: [SpectrumKey] {
        guard selectedMode == "Z" || spectrumBoxVisible || spectraLink.isOpen, canUseSpectra,
              let cube = cubeSource, let axis = spectralAxisIndex else { return [] }
        var indices = animator.indices
        if indices.count != cube.axes.count { indices = Array(repeating: 0, count: cube.axes.count) }
        let planes = (0..<cube.axes[axis].length).map { k -> Int in
            var i = indices
            i[axis] = k
            return cube.planeIndex(i)
        }
        return resolvedSpectrumSources.compactMap { source in
            spectrumArea(for: source).map {
                SpectrumKey(token: loadToken, source: source, area: $0, planes: planes, axisIndex: axis)
            }
        }
    }

    /// Computes the spectra for `spectrumKeys` that aren't ready yet, off the main thread, one after
    /// another (run by .task(id:), so newer keys cancel it).
    func updateSpectrum() async {
        let keys = spectrumKeys
        guard !keys.isEmpty, let reader = signalImage else {
            spectrum.isComputing = false
            return
        }
        let missing = keys.filter { key in !spectrum.results.contains { $0.key == key } }
        guard !missing.isEmpty else {
            spectrum.isComputing = false
            return
        }
        // Region edits arrive many times a second while dragging: wait for a pause first.
        // A single pixel (hovering) is quick, so it goes straight away.
        let anyLarge = missing.contains { !$0.area.isSinglePixel }
        if anyLarge {
            try? await Task.sleep(for: .milliseconds(150))
            if Task.isCancelled { return }
        }
        spectrum.isComputing = true
        spectrum.showsProgress = anyLarge
        spectrum.progress = 0
        let model = spectrum
        let count = Double(missing.count)
        for (i, key) in missing.enumerated() {
            let done = Double(i)
            let job = Task.detached(priority: .userInitiated) {
                SpectrumCalculator.compute(reader: reader, key: key) { fraction in
                    Task { @MainActor in model.progress = (done + fraction) / count }
                }
            }
            let result = await withTaskCancellationHandler {
                await job.value
            } onCancel: {
                job.cancel()
            }
            guard !Task.isCancelled else { return }
            if let result { spectrum.store(result) }
        }
        spectrum.isComputing = false
    }

    /// What the spectrum views show.
    var spectrumDisplay: SpectrumDisplay? {
        guard let cube = cubeSource, let axisIndex = spectralAxisIndex else { return nil }
        let keys = spectrumKeys
        let sources = resolvedSpectrumSources
        let series = sources.enumerated().map { index, source -> SpectrumSeries in
            let area = spectrumArea(for: source)
            // The latest result for this spectrum (an earlier one stands in while the region moves).
            let result = keys.first { $0.source == source }.flatMap { spectrum.result(for: $0) }
            return SpectrumSeries(source: source,
                                  name: spectrumName(for: source),
                                  color: SpectrumPalette.color(index),
                                  values: result?.values(spectrum.statistic),
                                  counts: result?.count,
                                  isSinglePixel: area?.isSinglePixel ?? (source == .active),
                                  resultID: result?.id)
        }

        let placeholder: String?
        if series.contains(where: { $0.values != nil }) {
            placeholder = nil
        } else if sources == [.active], inspectedPixel == nil {
            placeholder = "Hover over or double-tap a pixel to see its spectrum."
        } else {
            placeholder = spectrum.isComputing ? "Reading every channel…" : nil
        }
        return SpectrumDisplay(axis: cube.axes[axisIndex],
                               series: series,
                               current: animator.index(onAxis: axisIndex),
                               statistic: spectrum.statistic,
                               unit: valueUnit,
                               placeholder: placeholder)
    }

    /// The orange line was dragged (or the graph tapped): show that channel.
    func setSpectrumChannel(_ channel: Int) {
        guard let axis = spectralAxisIndex else { return }
        animator.isPlaying = false
        animator.setIndex(channel, onAxis: axis)
    }

    /// Spectra mode's bottom dock.
    @ViewBuilder
    var spectraDock: some View {
        if let display = spectrumDisplay {
            SpectraDockPanel(model: spectrum,
                             display: display,
                             regions: regionStore.statsCandidates,
                             glassNamespace: dockNamespace,
                             onChannel: { setSpectrumChannel($0) },
                             onPopOut: { openSpectraWindow() })
        }
    }

    /// The top-right spectrum box (every mode but Spectra, once you've left Spectra mode).
    @ViewBuilder
    var spectrumBox: some View {
        if spectrumBoxVisible, let display = spectrumDisplay {
            SpectrumBox(model: spectrum,
                        display: display,
                        onChannel: { setSpectrumChannel($0) },
                        onClose: { withAnimation(.snappy) { spectrum.showBox = false } })
                .transition(.opacity.combined(with: .scale(scale: 0.92, anchor: .topTrailing)))
        }
    }

    // MARK: - Spectra window

    /// Opens the spectra in their own window (a sheet where extra windows aren't available).
    func openSpectraWindow() {
        pushSpectraLink()
        if UIApplication.shared.supportsMultipleScenes {
            openWindow(id: "spectra", value: spectraLink.id)
        } else {
            showSpectraSheet = true
        }
    }

    /// What the Spectra window shows; when it changes, the window is updated. Nil (nothing to do)
    /// while no Spectra window is open.
    var spectraLinkKey: SpectraLinkKey? {
        guard spectraLink.isOpen else { return nil }
        let display = canUseSpectra ? spectrumDisplay : nil
        return SpectraLinkKey(token: loadToken,
                              fileName: fileName,
                              series: display?.series.map { "\($0.name)|\($0.resultID?.uuidString ?? "-")" } ?? [],
                              sources: display?.sources ?? [],
                              statistic: spectrum.statistic,
                              current: display?.current,
                              unit: valueUnit,
                              placeholder: display?.placeholder,
                              regions: regionStore.statsCandidates)
    }

    /// Sends the current spectrum to the Spectra window.
    func pushSpectraLink() {
        let link = spectraLink
        link.model = spectrum
        link.fileName = fileName
        let display = canUseSpectra ? spectrumDisplay : nil
        link.display = display
        link.current = display?.current
        link.regions = regionStore.statsCandidates
        link.onChannel = { channel in
            link.current = channel
            setSpectrumChannel(channel)
        }
    }

    /// Hosts the Spectra sheet (only used where extra windows aren't available).
    var spectraSheetHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .sheet(isPresented: $showSpectraSheet, onDismiss: { restoreImageFocus() }) {
                SpectraWindowView(link: spectraLink, onClose: { showSpectraSheet = false })
            }
    }
}

/// Everything the Spectra window shows, so it's only updated when something changed.
struct SpectraLinkKey: Equatable {
    let token: UUID
    let fileName: String
    /// Each spectrum's name and result.
    let series: [String]
    let sources: [SpectrumSource]
    let statistic: SpectrumStatistic
    let current: Int?
    let unit: String
    let placeholder: String?
    let regions: [FITSRegion]
}
