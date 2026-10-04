//
//  FITSPhysical.swift
//  iFITS Start
//
//  Physical pixel values (BSCALE / BZERO / BLANK) and the display row flip.
//

import Foundation

/// FITS physical values: value = BZERO + BSCALE × stored, and BLANK → NaN for integer images.
nonisolated enum FITSPhysical {
    /// Reverses the row order, so the image displays with FITS row 1 at the bottom (as in CARTA).
    /// Display row r (0 = top) then holds FITS row (height − r).
    static func flipRows(_ pixels: [Float], width: Int, height: Int) -> [Float] {
        guard width > 0, height > 1, pixels.count >= width * height else { return pixels }
        var out = [Float](repeating: .nan, count: pixels.count)
        pixels.withUnsafeBufferPointer { src in
            out.withUnsafeMutableBufferPointer { dst in
                for r in 0..<height {
                    let s = (height - 1 - r) * width
                    let d = r * width
                    for c in 0..<width { dst[d + c] = src[s + c] }
                }
            }
        }
        return out
    }

    static func apply(_ raw: [Float], bscale: Double, bzero: Double, header: [String: String]) -> [Float] {
        let bitpix = Int(header["BITPIX"]?.trimmingCharacters(in: .whitespaces) ?? "") ?? 0
        let blank: Float? = bitpix > 0
            ? header["BLANK"].flatMap { Double($0.trimmingCharacters(in: .whitespaces)) }.map { Float($0) }
            : nil
        guard bscale != 1 || bzero != 0 || blank != nil else { return raw }

        let s = Float(bscale), z = Float(bzero)
        var out = raw
        out.withUnsafeMutableBufferPointer { buf in
            for i in 0..<buf.count {
                let r = buf[i]
                if let blank, r == blank {
                    buf[i] = .nan
                } else {
                    buf[i] = z + s * r
                }
            }
        }
        return out
    }
}
