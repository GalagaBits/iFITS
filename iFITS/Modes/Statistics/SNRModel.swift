//
//  SNRModel.swift
//  iFITS Start
//
//  What the SNR page is set to: the noise, the cutoff, and the progress of a calculation.
//

import SwiftUI

/// The settings and state of the SNR page (S mode, second page of the dock).
@MainActor
@Observable
final class SNRModel {
    enum NoiseKind: Hashable {
        /// One σ for every pixel and channel (`manualSigma`, in BUNIT).
        case manual
        /// An image HDU of the open file (its HDU number), e.g. ERR.
        case hdu(Int)
        /// An HDU of another file (`noiseFile`).
        case file
    }

    var kind: NoiseKind = .manual
    /// σ for `.manual`, in the image's units (BUNIT). 0 = not entered yet.
    var manualSigma: Double = 0
    /// Pixels with value / σ below this become NaN.
    var cutoff: Double = 3
    /// Noise read from another file ("Open Noise File…").
    var noiseFile: ExternalNoise? = nil

    var isCalculating = false
    /// 0…1 while calculating.
    var progress: Double = 0
    /// The last result or problem, shown under the buttons.
    var message: String? = nil

    /// A new image was opened. Picks its ERR extension when it has one that fits; keeps a noise
    /// file only if it still fits.
    func imageChanged(candidates: [FITSHDUInfo], signalAxes: [Int]) {
        message = nil
        progress = 0
        if let file = noiseFile, !NoiseShapeMatch.check(noise: file.reader.axes, signal: signalAxes).isUsable {
            noiseFile = nil
        }
        let err = candidates.first {
            $0.name.uppercased() == "ERR" && NoiseShapeMatch.check(noise: $0.axes, signal: signalAxes).isUsable
        }
        if let err {
            kind = .hdu(err.index)
        } else if kind == .file, noiseFile != nil {
            // keep the noise file
        } else {
            kind = .manual
        }
    }
}
