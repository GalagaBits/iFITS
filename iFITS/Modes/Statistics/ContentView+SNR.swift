//
//  ContentView+SNR.swift
//  iFITS Start
//
//  SNR page actions: choosing the noise, Calculate SNR, saving and opening the "_SNR_" file,
//  and the SNR shown in Pixel Info.
//

import SwiftUI

extension ContentView {
    // MARK: - Noise choices

    /// Image HDUs of the open file that can be the noise: not the one shown, nor an SNR map.
    var noiseHDUCandidates: [FITSHDUInfo] {
        loadedHDUs.filter { $0.isImage && $0.index != loadedHDUIndex && $0.name.uppercased() != "SNR" }
    }

    /// For an "_SNR_" file made by iFITS, what it holds (from its header).
    var snrProductInfo: String? {
        // Only the clipped image (primary HDU) of such a file; the SNR extension has SNRCUT too.
        guard loadedHDUIndex == 0, headerDict["SNRKEPT"] != nil,
              let cut = FITSHDUList.number(headerDict["SNRCUT"]) else { return nil }
        var text = "This image keeps pixels with SNR ≥ \(SNRCalculator.cutoffText(cut))"
        if let noise = headerDict["SNRNOISE"], !noise.isEmpty { text += " (noise: \(noise))" }
        if let kept = Int(headerDict["SNRKEPT"] ?? ""), let total = Int(headerDict["SNRTOTAL"] ?? ""), total > 0 {
            text += String(format: ": %ld of %ld pixels (%.1f%%)", kept, total, 100 * Double(kept) / Double(total))
        }
        return text + "."
    }

    func openNoiseFilePicker() {
        importKind = .noise
        showPicker = true
    }

    /// A noise file was picked: use its image HDU, or ask which one when it has several.
    func loadNoiseFile(url: URL) {
        let axes = signalImage?.axes ?? []
        let name = url.lastPathComponent
        DispatchQueue.global(qos: .userInitiated).async {
            let scoped = url.startAccessingSecurityScopedResource()
            defer { if scoped { url.stopAccessingSecurityScopedResource() } }
            do {
                let data = try Data(contentsOf: url, options: .mappedIfSafe)
                let images = FITSHDUList.scan(data).filter(\.isImage)
                // Only one image: copy it now, while the file is open.
                let single = images.count == 1 ? ExternalNoise(fileName: name, hdu: images[0], data: data) : nil
                DispatchQueue.main.async {
                    if images.isEmpty {
                        snr.message = "\(name) has no image HDU to use as noise."
                    } else if images.count == 1 {
                        useNoiseFile(single)
                    } else {
                        hduPicker = HDUPickerRequest(purpose: .noise, url: url, hdus: images, requiredAxes: axes)
                    }
                }
            } catch {
                let message = error.localizedDescription
                DispatchQueue.main.async { snr.message = "Couldn't open \(name): \(message)" }
            }
        }
    }

    /// The noise HDU chosen in the HDU picker.
    func attachNoiseFile(url: URL, hdu: FITSHDUInfo) {
        let name = url.lastPathComponent
        DispatchQueue.global(qos: .userInitiated).async {
            let scoped = url.startAccessingSecurityScopedResource()
            defer { if scoped { url.stopAccessingSecurityScopedResource() } }
            let noise = (try? Data(contentsOf: url, options: .mappedIfSafe))
                .flatMap { ExternalNoise(fileName: name, hdu: hdu, data: $0) }
            DispatchQueue.main.async { useNoiseFile(noise) }
        }
    }

    private func useNoiseFile(_ noise: ExternalNoise?) {
        guard let noise else {
            snr.message = "Couldn't read that noise image."
            return
        }
        let match = NoiseShapeMatch.check(noise: noise.reader.axes, signal: signalImage?.axes ?? [])
        if case .mismatch(let why) = match {
            snr.message = why
            return
        }
        snr.noiseFile = noise
        snr.kind = .file
        snr.message = nil
    }

    // MARK: - Calculate SNR

    /// SNR = value / noise for every pixel of every channel. Builds "<name>_SNR_<cutoff>.fits"
    /// (pixels below the cutoff set to NaN, plus an SNR extension, the header, annotations and
    /// regions), asks where to save it, then opens it.
    func calculateSNR() {
        guard !snr.isCalculating else { return }
        guard let signal = signalImage,
              let signalHDU = loadedHDUs.first(where: { $0.index == loadedHDUIndex }) else {
            snr.message = "Open an image first."
            return
        }
        let noise: NoiseMap
        switch snr.kind {
        case .manual:
            guard snr.manualSigma.isFinite, snr.manualSigma > 0 else {
                snr.message = "Enter a noise value above 0."
                return
            }
            noise = NoiseMap(constant: snr.manualSigma, unit: valueUnit)
        case .hdu(let index):
            guard let hdu = loadedHDUs.first(where: { $0.index == index }),
                  let reader = FITSImageReader(data: signal.data, hdu: hdu),
                  let map = NoiseMap(image: reader, label: "\(hdu.label) of this file", signalAxes: signal.axes) else {
                snr.message = "That HDU can't be used as the noise. Check that its size fits the image."
                return
            }
            noise = map
        case .file:
            guard let file = snr.noiseFile,
                  let map = NoiseMap(image: file.reader, label: file.label, signalAxes: signal.axes) else {
                snr.message = "Open a noise file whose size fits the image."
                return
            }
            noise = map
        }
        let cutoff = snr.cutoff
        guard cutoff.isFinite else { return }

        // Same annotations and regions as the image on screen.
        let appendix = FITSAnnotationStore.extensionHDUs(drawing: annotations.drawingData(),
                                                         regionLines: DS9Regions.fitsLines(regionStore.regions))
        let sourceName = fileName
        // The image this result is for (another file may be opened while it's calculating).
        let token = loadToken
        let outputName = SNRCalculator.fileName(base: (fileName as NSString).deletingPathExtension, cutoff: cutoff)
        let model = snr
        model.isCalculating = true
        model.progress = 0
        model.message = nil

        DispatchQueue.global(qos: .userInitiated).async {
            let output = SNRCalculator.build(signal: signal, signalCards: signalHDU.cards, noise: noise,
                                             cutoff: cutoff, sourceFile: sourceName,
                                             sourceHDU: signalHDU.index, appendix: appendix) { fraction in
                DispatchQueue.main.async { model.progress = fraction }
            }
            DispatchQueue.main.async {
                model.isCalculating = false
                // Another file was opened meanwhile: this result belongs to the old one.
                guard loadToken == token else {
                    model.progress = 0
                    model.message = "Another image was opened while calculating, so that SNR result was discarded."
                    return
                }
                guard let output else {
                    model.message = "Couldn't read the image data to calculate SNR."
                    return
                }
                let percent = output.total > 0 ? 100 * Double(output.kept) / Double(output.total) : 0
                model.message = String(format: "%ld of %ld pixels (%.1f%%) have SNR ≥ %@. Choose where to save %@.fits.",
                                       output.kept, output.total, percent,
                                       SNRCalculator.cutoffText(cutoff), outputName)
                snrExportName = outputName + ".fits"
                snrExportDocument = FITSFileDocument(data: output.file)
                showSNRExporter = true
            }
        }
    }

    /// The SNR file was saved: open it in place of the current image, keeping the view and channel.
    func openSavedSNRFile(_ url: URL) {
        snrExportDocument = nil
        showSaveMessage("Saved \(url.lastPathComponent)")
        loadFITSFile(url: url, hdu: 0, keepView: true)
    }

    // MARK: - Pixel Info

    /// SNR at the inspected pixel, when the open file has an SNR extension (an "_SNR_" file).
    func snrValue(at pixel: InspectedPixel) -> Float? {
        guard let snrImage else { return nil }
        let plane = cubeSource?.planeIndex(animator.indices) ?? 0
        // Display row 0 is the top; FITS y (0-based) counts up from the bottom.
        return snrImage.value(x: pixel.column, y: snrImage.height - 1 - pixel.row, plane: plane)
    }
}
