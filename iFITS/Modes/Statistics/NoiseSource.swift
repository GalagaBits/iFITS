//
//  NoiseSource.swift
//  iFITS Start
//
//  The noise (σ) used for SNR: a single value, an HDU of the open file, or an HDU of another
//  file, plus the check that its size fits the image.
//

import Foundation

/// How a noise image's size fits the signal image.
nonisolated enum NoiseShapeMatch: Equatable, Sendable {
    /// Same width, height and channels: one σ per pixel per channel.
    case perChannel
    /// 2-D with the same width and height: the same σ map for every channel of the cube.
    case spatial
    /// Doesn't fit; the text says why.
    case mismatch(String)

    var isUsable: Bool {
        if case .mismatch = self { return false }
        return true
    }

    /// Compares NAXIS1, NAXIS2, … of the noise with the signal. Axes of length 1 at the end don't count.
    static func check(noise: [Int], signal: [Int]) -> NoiseShapeMatch {
        guard noise.count >= 2, signal.count >= 2 else {
            return .mismatch("The noise must be an image with at least two axes.")
        }
        guard noise[0] == signal[0], noise[1] == signal[1] else {
            return .mismatch("The noise is \(shape(noise)) pixels but the image is \(shape(signal)). "
                             + "The width and height must match.")
        }
        let noiseChannels = trimmed(Array(noise.dropFirst(2)))
        let signalChannels = trimmed(Array(signal.dropFirst(2)))
        if noiseChannels.isEmpty {
            return signalChannels.isEmpty ? .perChannel : .spatial
        }
        if noiseChannels == signalChannels { return .perChannel }
        return .mismatch("The noise has \(noiseChannels.reduce(1, *)) channels but the image has "
                         + "\(signalChannels.reduce(1, *)). Use noise with the same channels, or a 2-D noise image.")
    }

    /// "45 × 47 × 1213"
    static func shape(_ axes: [Int]) -> String {
        axes.map(String.init).joined(separator: " × ")
    }

    private static func trimmed(_ axes: [Int]) -> [Int] {
        var a = axes
        while a.last == 1 { a.removeLast() }
        return a
    }
}

/// A noise HDU read from another FITS file. Its pixels are copied, so that file can be closed.
nonisolated struct ExternalNoise: Sendable {
    let fileName: String
    let reader: FITSImageReader

    init?(fileName: String, hdu: FITSHDUInfo, data: Data) {
        guard let reader = FITSImageReader(copying: hdu, from: data) else { return nil }
        self.fileName = fileName
        self.reader = reader
    }

    /// "2: ERR of noise.fits"
    var label: String { "\(reader.label) of \(fileName)" }
}

/// σ for every pixel of every plane of the signal image.
nonisolated struct NoiseMap: Sendable {
    private enum Kind: Sendable {
        case constant(Float)
        case image(FITSImageReader, perChannel: Bool)
    }
    private let kind: Kind
    /// What the noise is, for the FITS header and messages ("2: ERR of this file", "1.5 MJy/sr").
    let label: String

    /// One σ for every pixel and channel.
    init(constant sigma: Double, unit: String) {
        kind = .constant(Float(sigma))
        label = unit.isEmpty ? String(format: "%g", sigma) : String(format: "%g ", sigma) + unit
    }

    /// Per-pixel σ from an image. nil if its size doesn't fit the signal.
    init?(image: FITSImageReader, label: String, signalAxes: [Int]) {
        switch NoiseShapeMatch.check(noise: image.axes, signal: signalAxes) {
        case .perChannel: kind = .image(image, perChannel: true)
        case .spatial: kind = .image(image, perChannel: false)
        case .mismatch: return nil
        }
        self.label = label
    }

    /// The single σ, when there is one.
    var constant: Float? {
        if case .constant(let sigma) = kind { return sigma }
        return nil
    }

    /// σ for signal plane `plane` (FITS order). nil for a constant σ (use `constant`) or a read error.
    func plane(_ plane: Int) -> [Float]? {
        guard case .image(let reader, let perChannel) = kind else { return nil }
        return reader.plane(perChannel ? plane : 0)
    }
}
