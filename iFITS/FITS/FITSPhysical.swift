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

    /// Reads one image plane straight from the file's bytes into display pixels, in one pass:
    /// big-endian FITS values → Float, physical values (BZERO + BSCALE × value, BLANK → NaN) and
    /// rows flipped (FITS row 1 at the bottom). Same result as `flipRows(apply(decoded))`, but it
    /// makes one array instead of three and uses every CPU core, in a few big chunks of rows.
    ///
    /// `bytes` points at the plane's first value; it must hold width × height × |BITPIX|/8 bytes.
    static func displayPixels(from bytes: UnsafeRawPointer, bitpix: Int, width: Int, height: Int,
                              bscale: Double, bzero: Double, header: [String: String]) -> [Float] {
        let n = width * height
        guard width > 0, height > 0 else { return [] }
        let blank: Float? = bitpix > 0
            ? header["BLANK"].flatMap { Double($0.trimmingCharacters(in: .whitespaces)) }.map { Float($0) }
            : nil
        let scaled = bscale != 1 || bzero != 0
        let job = PlaneJob(src: bytes, bitpix: bitpix, width: width, height: height,
                           scale: Float(bscale), zero: Float(bzero), scaled: scaled, blank: blank)
        // A few chunks per core (not one task per pixel: scheduling tiny tasks costs more than the work).
        let chunks = n < 65_536 ? 1 : min(height, max(1, ProcessInfo.processInfo.activeProcessorCount * 4))

        // Filled without zeroing it first (every value is written below).
        return [Float](unsafeUninitializedCapacity: n) { buffer, count in
            guard let out = buffer.baseAddress else { count = 0; return }
            let target = PlaneOutput(out: out)
            DispatchQueue.concurrentPerform(iterations: chunks) { c in
                let first = c * height / chunks, last = (c + 1) * height / chunks
                for row in first..<last { job.decodeRow(row, into: target.out) }
            }
            count = n
        }
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

/// Where `displayPixels` writes (shared by the parallel chunks; each writes only its own rows).
nonisolated private struct PlaneOutput: @unchecked Sendable {
    let out: UnsafeMutablePointer<Float>
}

/// One plane being read by `displayPixels`: the file's bytes and how to turn them into values.
nonisolated private struct PlaneJob: @unchecked Sendable {
    let src: UnsafeRawPointer
    let bitpix: Int
    let width: Int
    let height: Int
    let scale: Float
    let zero: Float
    let scaled: Bool
    let blank: Float?

    /// Display row `row` (0 = top) holds FITS row height − row, as in flipRows.
    func decodeRow(_ row: Int, into out: UnsafeMutablePointer<Float>) {
        let dst = out.advanced(by: row * width)
        let first = (height - 1 - row) * width          // first value of the FITS row, in the file
        let w = width
        // The BITPIX switch is outside the loops, so each loop is a tight, simple copy.
        switch bitpix {
        case 8:
            for c in 0..<w { dst[c] = Float(src.load(fromByteOffset: first + c, as: UInt8.self)) }
        case 16:
            for c in 0..<w {
                dst[c] = Float(Int16(bitPattern: UInt16(bigEndian: src.loadUnaligned(fromByteOffset: (first + c) * 2, as: UInt16.self))))
            }
        case 32:
            for c in 0..<w {
                dst[c] = Float(Int32(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: (first + c) * 4, as: UInt32.self))))
            }
        case 64:
            for c in 0..<w {
                dst[c] = Float(Int64(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: (first + c) * 8, as: UInt64.self))))
            }
        case -32:
            for c in 0..<w {
                dst[c] = Float(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: (first + c) * 4, as: UInt32.self)))
            }
        case -64:
            for c in 0..<w {
                dst[c] = Float(Double(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: (first + c) * 8, as: UInt64.self))))
            }
        default:
            for c in 0..<w { dst[c] = 0 }
        }
        // Physical values, while the row is still in the CPU's cache (same maths as apply).
        if let blank {
            for c in 0..<w {
                let r = dst[c]
                dst[c] = r == blank ? .nan : (scaled ? zero + scale * r : r)
            }
        } else if scaled {
            for c in 0..<w { dst[c] = zero + scale * dst[c] }
        }
    }
}
