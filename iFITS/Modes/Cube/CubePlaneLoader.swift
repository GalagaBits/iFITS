//
//  CubePlaneLoader.swift
//  iFITS Start
//
//  Reads one plane (channel) of a cube from the file on demand.
//

import SwiftUI
import UIKit

/// Raw pointers for the parallel plane decoder. Each chunk writes only its own part of `out`.
nonisolated private struct PlaneBuffers: @unchecked Sendable {
    let src: UnsafeRawPointer
    let out: UnsafeMutablePointer<Float>

    /// Big-endian FITS values [lo, hi) → Float.
    func decode(from lo: Int, to hi: Int, bitpix: Int) {
        guard lo < hi else { return }
        switch bitpix {
        case 8:
            for i in lo..<hi { out[i] = Float(src.load(fromByteOffset: i, as: UInt8.self)) }
        case 16:
            for i in lo..<hi {
                out[i] = Float(Int16(bitPattern: UInt16(bigEndian: src.loadUnaligned(fromByteOffset: i * 2, as: UInt16.self))))
            }
        case 32:
            for i in lo..<hi {
                out[i] = Float(Int32(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: i * 4, as: UInt32.self))))
            }
        case 64:
            for i in lo..<hi {
                out[i] = Float(Int64(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: i * 8, as: UInt64.self))))
            }
        case -32:
            for i in lo..<hi {
                out[i] = Float(bitPattern: UInt32(bigEndian: src.loadUnaligned(fromByteOffset: i * 4, as: UInt32.self)))
            }
        case -64:
            for i in lo..<hi {
                out[i] = Float(Double(bitPattern: UInt64(bigEndian: src.loadUnaligned(fromByteOffset: i * 8, as: UInt64.self))))
            }
        default:
            break
        }
    }
}

/// The loaded cube: the file's bytes (memory-mapped) plus where each plane is.
nonisolated struct CubeSource: Sendable {
    let data: Data
    let width: Int
    let height: Int
    let bitpix: Int
    let dataOffset: Int
    let bscale: Double
    let bzero: Double
    let header: [String: String]
    /// NAXIS3, NAXIS4, …
    let axes: [CubeAxis]

    var planeCount: Int { axes.reduce(1) { $0 * $1.length } }

    /// nil unless the image has more than one plane.
    init?(data: Data, width: Int, height: Int, bitpix: Int, dataOffset: Int,
          bscale: Double, bzero: Double, axisLengths: [Int], header: [String: String]) {
        guard axisLengths.count >= 3, width > 0, height > 0,
              [8, 16, 32, 64, -32, -64].contains(bitpix) else { return nil }
        let axes = (3...axisLengths.count).map {
            CubeAxis(number: $0, length: axisLengths[$0 - 1], header: header)
        }
        guard axes.reduce(1, { $0 * $1.length }) > 1 else { return nil }
        self.data = data
        self.width = width
        self.height = height
        self.bitpix = bitpix
        self.dataOffset = dataOffset
        self.bscale = bscale
        self.bzero = bzero
        self.header = header
        self.axes = axes
    }

    /// Plane number for a channel index along each axis (FITS order: NAXIS3 varies fastest).
    func planeIndex(_ indices: [Int]) -> Int {
        var plane = 0, stride = 1
        for (axis, index) in zip(axes, indices) {
            plane += min(max(index, 0), axis.length - 1) * stride
            stride *= axis.length
        }
        return plane
    }

    /// One plane in physical units, rows flipped for display (like the first plane at load).
    func loadPlane(_ plane: Int) -> [Float]? {
        let n = width * height
        let bytes = abs(bitpix) / 8
        guard n > 0, plane >= 0, plane < planeCount else { return nil }
        let start = dataOffset + plane * n * bytes
        guard start >= 0, start + n * bytes <= data.count else { return nil }

        var raw = [Float](repeating: 0, count: n)
        let bitpix = self.bitpix
        data.withUnsafeBytes { (src: UnsafeRawBufferPointer) in
            raw.withUnsafeMutableBufferPointer { dst in
                guard let srcBase = src.baseAddress, let dstBase = dst.baseAddress else { return }
                let buffers = PlaneBuffers(src: srcBase.advanced(by: start), out: dstBase)
                let chunks = max(1, min(16, n / 65_536))
                DispatchQueue.concurrentPerform(iterations: chunks) { c in
                    buffers.decode(from: c * n / chunks, to: (c + 1) * n / chunks, bitpix: bitpix)
                }
            }
        }
        let physical = FITSPhysical.apply(raw, bscale: bscale, bzero: bzero, header: header)
        return FITSPhysical.flipRows(physical, width: width, height: height)
    }
}

nonisolated struct CubePlaneRequest: Sendable {
    let source: CubeSource
    let plane: Int
    let settings: RenderSettings
    /// Clip percentile to apply to this plane (nil = keep the manual clip).
    let percentile: Double?
}

nonisolated struct CubePlaneResult: @unchecked Sendable {
    let plane: Int
    let pixels: [Float]
    let stats: ImageStats
    /// Settings the image was drawn with (clip range from this plane when using a percentile).
    let settings: RenderSettings
    let image: UIImage?
}

/// Decodes, measures and draws cube planes off the main thread, one at a time. While one is
/// in progress only the newest request is kept, so scrubbing and playback never pile up work.
@MainActor
final class CubePlaneLoader {
    var onPlane: ((CubePlaneResult) -> Void)?

    private var busy = false
    private var pending: CubePlaneRequest?
    private var generation = 0

    /// Forget anything in progress (a new file was opened).
    func reset() {
        generation += 1
        pending = nil
    }

    func request(_ request: CubePlaneRequest) {
        if busy { pending = request } else { start(request) }
    }

    private func start(_ request: CubePlaneRequest) {
        busy = true
        let gen = generation
        Task.detached(priority: .userInitiated) {
            let result = CubePlaneLoader.load(request)
            await self.finish(result, generation: gen)
        }
    }

    private func finish(_ result: CubePlaneResult?, generation gen: Int) {
        busy = false
        if gen == generation, let result { onPlane?(result) }
        if let next = pending {
            pending = nil
            start(next)
        }
    }

    nonisolated static func load(_ request: CubePlaneRequest) -> CubePlaneResult? {
        let source = request.source
        guard let pixels = source.loadPlane(request.plane) else { return nil }
        let stats = ImageStats(values: pixels, width: source.width)
        var settings = request.settings
        if let p = request.percentile {
            (settings.clipMin, settings.clipMax) = stats.clipRange(percentile: p)
        }
        let image = FITSRenderer.render(pixels, width: source.width, height: source.height, settings: settings)
        return CubePlaneResult(plane: request.plane, pixels: pixels, stats: stats, settings: settings, image: image)
    }
}
