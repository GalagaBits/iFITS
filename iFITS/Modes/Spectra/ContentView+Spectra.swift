//
//  ContentView+Spectra.swift
//  iFITS Start
//
//  Spectra mode ("Z", like CARTA's Z profile): the spectrum of the Active pixel, the entire image or
//  a region along the cube's spectral axis.
//

import SwiftUI

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

    /// What the spectrum is taken over now. A deleted region falls back to the Active pixel.
    var resolvedSpectrumSource: SpectrumSource {
        if case .region(let id) = spectrum.source {
            guard let region = regionStore.region(id), region.shape.hasStatistics else { return .active }
        }
        return spectrum.source
    }

    /// The pixels of the spectrum, or nil (no Active pixel yet).
    var spectrumArea: SpectrumArea? {
        switch resolvedSpectrumSource {
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

    /// The spectrum is on screen: the Spectra dock, or the top-right box.
    var spectrumBoxVisible: Bool {
        spectrum.showBox && canUseSpectra && selectedMode != "Z"
    }

    /// What to compute (nil when no spectrum is on screen). Changing channel doesn't change it; the
    /// region, the Active pixel, or another Stokes (or other axis) channel does.
    var spectrumKey: SpectrumKey? {
        guard selectedMode == "Z" || spectrumBoxVisible, canUseSpectra,
              let cube = cubeSource, let axis = spectralAxisIndex, let area = spectrumArea else { return nil }
        var indices = animator.indices
        if indices.count != cube.axes.count { indices = Array(repeating: 0, count: cube.axes.count) }
        let planes = (0..<cube.axes[axis].length).map { k -> Int in
            var i = indices
            i[axis] = k
            return cube.planeIndex(i)
        }
        return SpectrumKey(token: loadToken, area: area, planes: planes, axisIndex: axis)
    }

    /// Computes the spectrum for `spectrumKey` off the main thread (run by .task(id:), so a newer
    /// key cancels it).
    func updateSpectrum() async {
        guard let key = spectrumKey, let reader = signalImage else {
            spectrum.isComputing = false
            return
        }
        if spectrum.result?.key == key {
            spectrum.isComputing = false
            return
        }
        // Region edits arrive many times a second while dragging: wait for a pause first.
        // A single pixel (hovering) is quick, so it goes straight away.
        let single = key.area.isSinglePixel
        if !single {
            try? await Task.sleep(for: .milliseconds(150))
            if Task.isCancelled { return }
        }
        spectrum.isComputing = true
        spectrum.showsProgress = !single
        spectrum.progress = 0
        let model = spectrum
        let job = Task.detached(priority: .userInitiated) {
            SpectrumCalculator.compute(reader: reader, key: key) { fraction in
                Task { @MainActor in model.progress = fraction }
            }
        }
        let result = await withTaskCancellationHandler {
            await job.value
        } onCancel: {
            job.cancel()
        }
        guard !Task.isCancelled else { return }
        spectrum.isComputing = false
        if let result {
            // A new number of channels (another file) starts zoomed out.
            if spectrum.result?.channelCount != result.channelCount { spectrum.zoom = nil }
            spectrum.result = result
        }
    }

    /// What the spectrum views show.
    var spectrumDisplay: SpectrumDisplay? {
        guard let cube = cubeSource, let axisIndex = spectralAxisIndex else { return nil }
        let axis = cube.axes[axisIndex]
        let source = resolvedSpectrumSource
        let area = spectrumArea
        let key = spectrumKey

        let name: String
        switch source {
        case .active:
            if let pixel = inspectedPixel {
                name = "Active (x \(pixel.column + 1), y \(Int(imageHeight) - pixel.row))"
            } else {
                name = "Active Pixel"
            }
        case .entireImage:
            name = "Entire Image"
        case .region(let id):
            name = regionStore.region(id)?.name ?? "Region"
        }

        // Show the latest result only if it's for what's chosen now (kept while the next one is
        // computed when only the region moved, so the graph doesn't blink).
        var result = spectrum.result(for: loadToken)
        if let r = result, let key, r.key.planes != key.planes || !sameKind(r.key.area, key.area) {
            result = nil
        }
        if key == nil { result = nil }

        let placeholder: String?
        if area == nil {
            placeholder = "Hover over or double-tap a pixel to see its spectrum."
        } else if result == nil {
            placeholder = spectrum.isComputing ? "Reading every channel…" : nil
        } else {
            placeholder = nil
        }
        let single = area?.isSinglePixel ?? (source == .active)
        return SpectrumDisplay(axis: axis,
                               source: source,
                               values: result?.values(spectrum.statistic),
                               counts: result?.count,
                               current: animator.index(onAxis: axisIndex),
                               sourceName: name,
                               isSinglePixel: single,
                               statistic: spectrum.statistic,
                               unit: valueUnit,
                               placeholder: placeholder)
    }

    /// Both areas are the same kind (pixel / image / same region shape), so an earlier spectrum can
    /// stand in until the new one is ready.
    private func sameKind(_ a: SpectrumArea, _ b: SpectrumArea) -> Bool {
        switch (a, b) {
        case (.pixel, .pixel), (.entireImage, .entireImage): return true
        case (.region(let s1, _, _, _), .region(let s2, _, _, _)): return s1 == s2
        default: return false
        }
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
                             onChannel: { setSpectrumChannel($0) })
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
}
