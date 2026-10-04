//
//  SNRCalculator.swift
//  iFITS Start
//
//  SNR = value / σ for every pixel of every channel, and the "_SNR_<cutoff>" FITS file:
//  the image with pixels below the cutoff set to NaN, plus an SNR extension.
//

import Foundation

nonisolated enum SNRCalculator {
    struct Output: Sendable {
        /// The new FITS file, ready to save.
        let file: Data
        /// Pixels with SNR ≥ cutoff (kept), pixels with a finite SNR, and all pixels.
        let kept: Int
        let measured: Int
        let total: Int
    }

    /// SNR keywords from an earlier run, never copied into a new file (they'd be out of date).
    private static let staleKeywords: Set<String> = [
        "SNRCUT", "SNRNOISE", "SNRKEPT", "SNRTOTAL", "ORIGFILE", "ORIGHDU", "NEXTEND"
    ]

    /// "image_SNR_3" (no extension).
    static func fileName(base: String, cutoff: Double) -> String {
        base + "_SNR_" + cutoffText(cutoff)
    }

    /// 3 → "3", 2.5 → "2.5".
    static func cutoffText(_ cutoff: Double) -> String {
        String(format: "%g", cutoff)
    }

    /// Calculates SNR for every pixel of every plane and builds the new file:
    /// • Primary HDU: the image, with pixels whose SNR is below `cutoff` (or can't be measured)
    ///   set to NaN. Its header is a copy of the image's header (WCS, units, …) with new layout
    ///   cards (BITPIX = -32) and SNRCUT, SNRNOISE, SNRKEPT, SNRTOTAL, ORIGFILE, ORIGHDU added.
    /// • "SNR" image extension: value / σ for every pixel (before the cutoff), with the same WCS.
    /// • `appendix`: HDUs added at the end as they are (the annotation and region extensions).
    /// SNR can't be measured where the value or σ is NaN, or σ ≤ 0; those pixels are NaN in both.
    /// The file is written straight into one buffer, so memory use stays close to the file's size.
    /// - signalCards: the image HDU's header cards (FITSHDUInfo.cards).
    /// - progress: called now and then with 0…1, from this (background) thread.
    static func build(signal: FITSImageReader, signalCards: [String], noise: NoiseMap, cutoff: Double,
                      sourceFile: String, sourceHDU: Int, appendix: Data = Data(),
                      progress: (Double) -> Void = { _ in }) -> Output? {
        let width = signal.width, height = signal.height
        let n = width * height
        let planes = signal.planeCount
        guard n > 0, planes > 0 else { return nil }
        let planeBytes = n * 4
        let dataBytes = planeBytes * planes
        let total = n * planes
        let noiseText = noise.label

        // Headers. The counts sit in fixed-width fields, so the primary header is the same size
        // before and after the counts are known: it's written once now and again at the end.
        func primaryHeader(kept: Int) -> Data {
            let history = "iFITS: pixels with value / noise below \(cutoffText(cutoff)) set to NaN "
                + "(noise: \(noiseText))."
            return FITSImageWriter.headerData(FITSImageWriter.imageHeader(
                copying: signalCards, primary: true, axes: signal.axes, extname: nil,
                alsoDrop: staleKeywords,
                extra: [
                    FITSImageWriter.real("SNRCUT", cutoff, "Pixels with SNR below this are NaN"),
                    FITSImageWriter.string("SNRNOISE", noiseText, "Noise used for SNR = value / noise"),
                    FITSImageWriter.int("SNRKEPT", kept, "Pixels with SNR >= SNRCUT"),
                    FITSImageWriter.int("SNRTOTAL", total, "All pixels"),
                    FITSImageWriter.string("ORIGFILE", sourceFile, "File the image came from"),
                    FITSImageWriter.int("ORIGHDU", sourceHDU, "HDU the image came from (0 = primary)")
                ] + FITSImageWriter.text("HISTORY", history)))
        }
        let snrHeader = FITSImageWriter.headerData(FITSImageWriter.imageHeader(
            copying: signalCards, primary: false, axes: signal.axes, extname: "SNR",
            alsoDrop: staleKeywords.union(["BUNIT", "EXTVER"]),
            extra: [
                FITSImageWriter.real("SNRCUT", cutoff, "Cutoff used for the primary image"),
                FITSImageWriter.string("SNRNOISE", noiseText, "Noise used for SNR = value / noise")
            ] + FITSImageWriter.text("COMMENT", "Signal-to-noise ratio (value / noise) of every pixel, before the cutoff.")))

        // Layout: primary header | image data + padding | SNR header | SNR data + padding | appendix.
        let headerSize = primaryHeader(kept: 0).count
        let paddedData = dataBytes + FITSImageWriter.dataPadding(for: dataBytes).count
        let imageStart = headerSize
        let snrHeaderStart = imageStart + paddedData
        let snrStart = snrHeaderStart + snrHeader.count
        let baseSize = snrStart + paddedData

        var file = Data(capacity: baseSize + appendix.count)
        file.count = baseSize                                 // zero-filled: the data padding
        file.replaceSubrange(snrHeaderStart..<snrStart, with: snrHeader)

        var kept = 0, measured = 0
        let cut = Float(cutoff)
        let constant = noise.constant
        let reportEvery = max(1, planes / 100)

        for p in 0..<planes {
            guard let values = signal.plane(p) else { return nil }
            let sigmas: [Float] = constant == nil ? (noise.plane(p) ?? []) : []
            if constant == nil && sigmas.count != n { return nil }

            var keptValues = [Float](repeating: .nan, count: n)
            var snrValues = [Float](repeating: .nan, count: n)
            values.withUnsafeBufferPointer { v in
                keptValues.withUnsafeMutableBufferPointer { k in
                    snrValues.withUnsafeMutableBufferPointer { s in
                        for i in 0..<n {
                            let sigma = constant ?? sigmas[i]
                            let value = v[i]
                            guard value.isFinite, sigma.isFinite, sigma > 0 else { continue }
                            let snr = value / sigma
                            guard snr.isFinite else { continue }
                            s[i] = snr
                            measured += 1
                            if snr >= cut {
                                k[i] = value
                                kept += 1
                            }
                        }
                    }
                }
            }
            FITSImageWriter.write(keptValues, into: &file, at: imageStart + p * planeBytes)
            FITSImageWriter.write(snrValues, into: &file, at: snrStart + p * planeBytes)
            if p % reportEvery == 0 || p == planes - 1 { progress(Double(p + 1) / Double(planes)) }
        }

        let header = primaryHeader(kept: kept)
        guard header.count == headerSize else { return nil }
        file.replaceSubrange(0..<headerSize, with: header)
        file.append(appendix)
        return Output(file: file, kept: kept, measured: measured, total: total)
    }
}
