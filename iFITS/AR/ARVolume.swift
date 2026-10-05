//
//  ARVolume.swift
//  iFITS Start
//
//  The cube as one 3-D block of raw pixel values, ready for the GPU.
//

import Foundation

/// The cube's raw (physical) values as a 3-D block: x = NAXIS1, y = NAXIS2, depth = the spectral
/// (or other) axis. Values are in FITS order: x fastest, then y (FITS y = 1 first), then depth.
/// Nothing is smoothed or averaged. A cube too big for the GPU keeps every n-th pixel or channel.
nonisolated struct ARVolume: Sendable {
    let values: [Float]
    /// Size of `values`.
    let width: Int
    let height: Int
    let depth: Int
    /// Every `spatialStep`-th pixel and `depthStep`-th channel is kept (1 = all of them).
    let spatialStep: Int
    let depthStep: Int
    /// Sorted sample of the finite values, for percentile ranges.
    let stats: ImageStats

    /// Pixels / channels the block covers in the original cube (its size × the steps).
    var extentX: Int { width * spatialStep }
    var extentY: Int { height * spatialStep }
    var extentZ: Int { depth * depthStep }
    var isThinned: Bool { spatialStep > 1 || depthStep > 1 }

    /// Apple GPUs take 3-D textures up to 2048 on a side; the voxel limit keeps memory reasonable.
    static let maxSide = 2048
    static let maxVoxels = 32_000_000

    /// Reads the planes `planes` (FITS plane numbers, one per channel along the depth axis).
    /// - progress: called with 0…1 from this (background) thread.
    static func build(reader: FITSImageReader, planes: [Int],
                      progress: (Double) -> Void = { _ in }) -> ARVolume? {
        let nx = reader.width, ny = reader.height, nz = planes.count
        guard nx > 0, ny > 0, nz > 0 else { return nil }

        // Thin only if needed: whole pixels / channels are skipped, never averaged.
        var s = max(1, (max(nx, ny) + maxSide - 1) / maxSide)
        var c = max(1, (nz + maxSide - 1) / maxSide)
        func size(_ n: Int, _ step: Int) -> Int { (n + step - 1) / step }
        while size(nx, s) * size(ny, s) * size(nz, c) > maxVoxels {
            if size(nz, c) >= max(size(nx, s), size(ny, s)) { c += 1 } else { s += 1 }
        }
        let dx = size(nx, s), dy = size(ny, s), dz = size(nz, c)

        var values = [Float](repeating: .nan, count: dx * dy * dz)
        let reportEvery = max(1, dz / 50)
        var ok = true
        values.withUnsafeMutableBufferPointer { out in
            for k in 0..<dz {
                // Stop early if the AR view was closed while reading.
                guard !Task.isCancelled, let plane = reader.plane(planes[k * c]), plane.count >= nx * ny else {
                    ok = false
                    return
                }
                plane.withUnsafeBufferPointer { src in
                    for j in 0..<dy {
                        let row = j * s * nx
                        let base = (k * dy + j) * dx
                        for i in 0..<dx {
                            out[base + i] = src[row + i * s]
                        }
                    }
                }
                if k % reportEvery == 0 || k == dz - 1 { progress(Double(k + 1) / Double(dz)) }
            }
        }
        guard ok else { return nil }
        return ARVolume(values: values, width: dx, height: dy, depth: dz,
                        spatialStep: s, depthStep: c, stats: ImageStats(values: values, width: dx))
    }
}
