//
//  FITSImageReader.swift
//  iFITS Start
//
//  Reads physical pixel values from any image HDU, one plane or one pixel at a time.
//

import Foundation

/// Reads an image HDU's pixels in physical units (BZERO + BSCALE × stored value, BLANK → NaN),
/// in FITS order: row 0 is FITS y = 1 (the bottom row), x varies fastest, then y, then the planes.
/// The data stays in the (usually memory-mapped) file until a plane or pixel is asked for.
nonisolated struct FITSImageReader: Sendable {
    let data: Data
    /// Byte offset of the first pixel in `data`.
    let dataStart: Int
    let bitpix: Int
    /// NAXIS1, NAXIS2, NAXIS3, …
    let axes: [Int]
    let bscale: Double
    let bzero: Double
    /// BLANK, for integer images.
    let blank: Int64?
    /// "2: ERR", for messages and the FITS header.
    let label: String
    /// BUNIT.
    let unit: String

    var width: Int { axes[0] }
    var height: Int { axes[1] }
    var planeCount: Int { axes.dropFirst(2).reduce(1, *) }
    private var bytesPerValue: Int { abs(bitpix) / 8 }

    /// Reads `hdu` from `data` (the whole file), without copying.
    init?(data: Data, hdu: FITSHDUInfo) {
        guard hdu.isImage, let size = FITSHDUList.product(hdu.axes + [abs(hdu.bitpix) / 8]) else { return nil }
        guard hdu.dataOffset >= 0, size <= data.count - hdu.dataOffset else { return nil }
        let blank: Int64? = hdu.bitpix > 0
            ? hdu.keys["BLANK"].flatMap { Int64($0.trimmingCharacters(in: .whitespaces)) }
            : nil
        self.init(data: data, dataStart: hdu.dataOffset, bitpix: hdu.bitpix, axes: hdu.axes,
                  bscale: hdu.bscale, bzero: hdu.bzero, blank: blank, label: hdu.label, unit: hdu.unit)
    }

    /// Copies just `hdu`'s pixels out of `data`, so the file can be closed afterwards.
    init?(copying hdu: FITSHDUInfo, from data: Data) {
        guard let reader = FITSImageReader(data: data, hdu: hdu) else { return nil }
        let size = reader.bytesPerValue * reader.axes.reduce(1, *)
        let copy = data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) -> Data in
            guard let base = raw.baseAddress else { return Data() }
            return Data(bytes: base + reader.dataStart, count: size)
        }
        guard copy.count == size else { return nil }
        self.init(data: copy, dataStart: 0, bitpix: reader.bitpix, axes: reader.axes,
                  bscale: reader.bscale, bzero: reader.bzero, blank: reader.blank,
                  label: reader.label, unit: reader.unit)
    }

    private init(data: Data, dataStart: Int, bitpix: Int, axes: [Int], bscale: Double, bzero: Double,
                 blank: Int64?, label: String, unit: String) {
        self.data = data
        self.dataStart = dataStart
        self.bitpix = bitpix
        self.axes = axes
        self.bscale = bscale
        self.bzero = bzero
        self.blank = blank
        self.label = label
        self.unit = unit
    }

    /// One plane (0-based) in physical units, FITS order (not flipped for display).
    func plane(_ plane: Int) -> [Float]? {
        let n = width * height
        let bytes = bytesPerValue
        guard n > 0, plane >= 0, plane < planeCount else { return nil }
        let start = dataStart + plane * n * bytes
        guard start >= 0, start + n * bytes <= data.count else { return nil }
        var out = [Float](repeating: .nan, count: n)
        data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) in
            guard let base = raw.baseAddress else { return }
            out.withUnsafeMutableBufferPointer { dst in
                guard let dstBase = dst.baseAddress else { return }
                decode(base + start, count: n, into: dstBase)
            }
        }
        return out
    }

    /// One pixel in physical units. `x`, `y` and `plane` are 0-based, `y` counted from the bottom row.
    func value(x: Int, y: Int, plane: Int = 0) -> Float? {
        guard x >= 0, y >= 0, x < width, y < height, plane >= 0, plane < planeCount else { return nil }
        let offset = dataStart + ((plane * height + y) * width + x) * bytesPerValue
        guard offset + bytesPerValue <= data.count else { return nil }
        var v = Float.nan
        data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) in
            guard let base = raw.baseAddress else { return }
            decode(base + offset, count: 1, into: &v)
        }
        return v
    }

    /// Physical values of several runs of pixels in one plane (Spectra mode reads a region this way,
    /// channel by channel). Each run is `count` pixels in a row starting at FITS-order index
    /// `start` (y × width + x, 0-based, y from the bottom row). Values go to `out` one run after
    /// another. False if the plane or a run is outside the image.
    func values(plane: Int, runs: [PixelRun], into out: UnsafeMutablePointer<Float>) -> Bool {
        let n = width * height
        let bytes = bytesPerValue
        guard n > 0, plane >= 0, plane < planeCount else { return false }
        let start = dataStart + plane * n * bytes
        guard start >= 0, start + n * bytes <= data.count else { return false }
        return data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) -> Bool in
            guard let base = raw.baseAddress else { return false }
            var o = 0
            for run in runs {
                guard run.start >= 0, run.count > 0, run.start + run.count <= n else { return false }
                decode(base + start + run.start * bytes, count: run.count, into: out + o)
                o += run.count
            }
            return true
        }
    }

    /// Big-endian stored values → physical Floats.
    private func decode(_ src: UnsafeRawPointer, count n: Int, into out: UnsafeMutablePointer<Float>) {
        let s = bscale, z = bzero
        let scaled = s != 1 || z != 0
        let blank = self.blank
        func integer(_ r: Int64) -> Float {
            if let blank, r == blank { return .nan }
            return scaled ? Float(z + s * Double(r)) : Float(r)
        }
        switch bitpix {
        case 8:
            for i in 0..<n { out[i] = integer(Int64(src.load(fromByteOffset: i, as: UInt8.self))) }
        case 16:
            for i in 0..<n {
                out[i] = integer(Int64(Int16(bitPattern: UInt16(bigEndian: src.loadUnaligned(fromByteOffset: i * 2, as: UInt16.self)))))
            }
        case 32:
            for i in 0..<n {
                out[i] = integer(Int64(Int32(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: i * 4, as: UInt32.self)))))
            }
        case 64:
            for i in 0..<n {
                out[i] = integer(Int64(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: i * 8, as: UInt64.self))))
            }
        case -32:
            for i in 0..<n {
                let v = Float(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: i * 4, as: UInt32.self)))
                out[i] = scaled ? Float(z + s * Double(v)) : v
            }
        case -64:
            for i in 0..<n {
                let v = Double(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: i * 8, as: UInt64.self)))
                out[i] = Float(scaled ? z + s * v : v)
            }
        default:
            for i in 0..<n { out[i] = .nan }
        }
    }
}
