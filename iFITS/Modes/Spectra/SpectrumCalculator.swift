//
//  SpectrumCalculator.swift
//  iFITS Start
//
//  Spectra (Z profile): which pixels a spectrum is taken over, and the per-channel statistics
//  (Sum, Mean, StdDev, Min, Max, RMS), read straight from the file off the main thread.
//

import Foundation
import CoreGraphics

/// `count` pixels in one row, starting at FITS-order index `start` within a plane
/// (y × width + x, 0-based, y counted from the bottom row).
nonisolated struct PixelRun: Equatable, Sendable {
    let start: Int
    let count: Int
}

/// The statistic plotted for an area of more than one pixel (same definitions as the S mode).
nonisolated enum SpectrumStatistic: String, CaseIterable, Identifiable, Sendable {
    case sum, mean, stdDev, min, max, rms

    var id: Self { self }

    var title: String {
        switch self {
        case .sum: "Sum"
        case .mean: "Mean"
        case .stdDev: "StdDev"
        case .min: "Min"
        case .max: "Max"
        case .rms: "RMS"
        }
    }
}

/// What the spectrum is taken over, as chosen in the menu.
nonisolated enum SpectrumSource: Hashable, Sendable {
    /// The pixel shown in Pixel Info (hovered or double-tapped), like CARTA's "Active" cursor.
    case active
    case entireImage
    case region(UUID)
}

/// The pixels a spectrum covers. Only the geometry, so renaming or recoloring a region doesn't
/// recompute anything.
nonisolated enum SpectrumArea: Equatable, Sendable {
    /// One pixel: 0-based FITS x and y (y counted from the bottom row).
    case pixel(x: Int, y: Int)
    case entireImage
    /// An ellipse, rectangle or point region, in FITS pixel coordinates (see FITSRegion).
    case region(shape: RegionShape, center: CGPoint, size: CGSize, angle: Double)

    init(_ region: FITSRegion) {
        self = .region(shape: region.shape, center: region.center, size: region.size, angle: region.angle)
    }

    /// One value per channel: no statistic to choose.
    var isSinglePixel: Bool {
        switch self {
        case .pixel: true
        case .region(let shape, _, _, _): shape == .point
        case .entireImage: false
        }
    }

    /// The covered pixels as runs along rows. Pixels count when their centers are inside the shape,
    /// exactly as in RegionStatistics. Lines (no area) and areas off the image give no runs.
    func runs(width: Int, height: Int) -> [PixelRun] {
        guard width > 0, height > 0 else { return [] }
        switch self {
        case .pixel(let x, let y):
            guard x >= 0, y >= 0, x < width, y < height else { return [] }
            return [PixelRun(start: y * width + x, count: 1)]

        case .entireImage:
            return (0..<height).map { PixelRun(start: $0 * width, count: width) }

        case .region(let shape, let center, let size, let angle):
            // Double → pixel number, safe for huge or non-finite values (as in RegionStatistics).
            func pixelIndex(_ v: Double, _ rule: FloatingPointRoundingRule) -> Int {
                guard v.isFinite else { return v > 0 ? Int.max / 2 : Int.min / 2 }
                return Int(Swift.min(Swift.max(v, -1e9), 1e9).rounded(rule))
            }
            let cx = Double(center.x), cy = Double(center.y)
            switch shape {
            case .line:
                return []
            case .point:
                let fx = pixelIndex(cx, .toNearestOrAwayFromZero)
                let fy = pixelIndex(cy, .toNearestOrAwayFromZero)
                guard fx >= 1, fy >= 1, fx <= width, fy <= height else { return [] }
                return [PixelRun(start: (fy - 1) * width + (fx - 1), count: 1)]
            case .ellipse, .rectangle:
                let radians = angle * .pi / 180
                let cosA = cos(radians), sinA = sin(radians)
                let hw = Double(size.width) / 2, hh = Double(size.height) / 2
                guard cx.isFinite, cy.isFinite, hw.isFinite, hh.isFinite else { return [] }
                let ex = abs(hw * cosA) + abs(hh * sinA)
                let ey = abs(hw * sinA) + abs(hh * cosA)
                let x0 = Swift.max(pixelIndex(cx - ex, .up), 1), x1 = Swift.min(pixelIndex(cx + ex, .down), width)
                let y0 = Swift.max(pixelIndex(cy - ey, .up), 1), y1 = Swift.min(pixelIndex(cy + ey, .down), height)
                guard x0 <= x1, y0 <= y1 else { return [] }
                let isEllipse = shape == .ellipse
                if isEllipse, !(hw > 0 && hh > 0) { return [] }

                var runs: [PixelRun] = []
                for fy in y0...y1 {
                    let dy = Double(fy) - cy
                    var runStart: Int? = nil
                    for fx in x0...x1 {
                        let dx = Double(fx) - cx
                        let lx = dx * cosA + dy * sinA
                        let ly = -dx * sinA + dy * cosA
                        let inside = isEllipse
                            ? (lx * lx) / (hw * hw) + (ly * ly) / (hh * hh) <= 1
                            : abs(lx) <= hw && abs(ly) <= hh
                        if inside {
                            if runStart == nil { runStart = fx }
                        } else if let s = runStart {
                            runs.append(PixelRun(start: (fy - 1) * width + (s - 1), count: fx - s))
                            runStart = nil
                        }
                    }
                    if let s = runStart {
                        runs.append(PixelRun(start: (fy - 1) * width + (s - 1), count: x1 - s + 1))
                    }
                }
                return runs
            }
        }
    }
}

/// What a spectrum was computed for. A new key starts a new computation.
nonisolated struct SpectrumKey: Equatable, Sendable {
    /// ContentView.loadToken of the image.
    let token: UUID
    /// The menu choice this spectrum is for (one line on the graph).
    let source: SpectrumSource
    let area: SpectrumArea
    /// FITS plane number of each channel along the spectral axis (other axes, e.g. Stokes, fixed).
    let planes: [Int]
    /// Index (into the cube's axes) of the spectral axis.
    let axisIndex: Int
}

/// Per-channel statistics of the pixels in an area. NaN where a channel has no finite pixels.
nonisolated struct SpectrumResult: Sendable {
    /// New for every result (the Spectra window updates when it changes).
    let id = UUID()
    let key: SpectrumKey
    let count: [Int]
    let sum: [Double]
    let mean: [Double]
    let stdDev: [Double]
    let min: [Double]
    let max: [Double]
    let rms: [Double]

    var channelCount: Int { count.count }

    /// The plotted values. A single pixel always shows its own value.
    func values(_ statistic: SpectrumStatistic) -> [Double] {
        if key.area.isSinglePixel { return mean }
        switch statistic {
        case .sum: return sum
        case .mean: return mean
        case .stdDev: return stdDev
        case .min: return min
        case .max: return max
        case .rms: return rms
        }
    }
}

/// Output arrays shared by the parallel workers. Each channel writes only its own index.
nonisolated private struct SpectrumBuffers: @unchecked Sendable {
    let count: UnsafeMutablePointer<Int>
    let sum: UnsafeMutablePointer<Double>
    let mean: UnsafeMutablePointer<Double>
    let stdDev: UnsafeMutablePointer<Double>
    let min: UnsafeMutablePointer<Double>
    let max: UnsafeMutablePointer<Double>
    let rms: UnsafeMutablePointer<Double>
}

nonisolated enum SpectrumCalculator {
    /// Most pixels read into memory at once per channel (runs are read in groups this size).
    static let groupSize = 262_144

    /// The spectrum for `key`, reading every channel of `reader`. Channels are spread over the
    /// CPU cores. Returns nil if the task was cancelled.
    /// - progress: 0…1, called from this (background) thread.
    static func compute(reader: FITSImageReader, key: SpectrumKey,
                        progress: (Double) -> Void = { _ in }) -> SpectrumResult? {
        let planes = key.planes
        let n = planes.count
        let runs = key.area.runs(width: reader.width, height: reader.height)

        // Runs in groups of at most `groupSize` pixels (a longer run is a group by itself).
        var groups: [[PixelRun]] = []
        var current: [PixelRun] = []
        var currentCount = 0
        for run in runs {
            if currentCount > 0, currentCount + run.count > groupSize {
                groups.append(current)
                current = []
                currentCount = 0
            }
            current.append(run)
            currentCount += run.count
        }
        if !current.isEmpty { groups.append(current) }
        let runGroups = groups
        let largestGroup = runGroups.map { $0.reduce(0) { $0 + $1.count } }.max() ?? 0

        let count = UnsafeMutablePointer<Int>.allocate(capacity: max(1, n))
        let doubles = (0..<6).map { _ in UnsafeMutablePointer<Double>.allocate(capacity: max(1, n)) }
        defer {
            count.deallocate()
            doubles.forEach { $0.deallocate() }
        }
        let out = SpectrumBuffers(count: count, sum: doubles[0], mean: doubles[1], stdDev: doubles[2],
                                  min: doubles[3], max: doubles[4], rms: doubles[5])

        let workers = Swift.max(1, Swift.min(8, ProcessInfo.processInfo.activeProcessorCount))
        // Bigger batches for small areas (cheap channels), smaller for big ones (progress, cancel).
        let batch = largestGroup <= 4096 ? Swift.max(workers, 256) : workers * 2
        var done = 0
        while done < n {
            if Task.isCancelled { return nil }
            let first = done
            let size = Swift.min(batch, n - first)
            DispatchQueue.concurrentPerform(iterations: size) { i in
                channel(first + i, plane: planes[first + i], reader: reader, groups: runGroups,
                        scratchSize: largestGroup, out: out)
            }
            done += size
            progress(Double(done) / Double(n))
        }
        if Task.isCancelled { return nil }

        func array(_ p: UnsafeMutablePointer<Double>) -> [Double] { Array(UnsafeBufferPointer(start: p, count: n)) }
        return SpectrumResult(key: key,
                              count: Array(UnsafeBufferPointer(start: count, count: n)),
                              sum: array(out.sum), mean: array(out.mean), stdDev: array(out.stdDev),
                              min: array(out.min), max: array(out.max), rms: array(out.rms))
    }

    /// Statistics of one channel, written to index `k` of `out`.
    private static func channel(_ k: Int, plane: Int, reader: FITSImageReader, groups: [[PixelRun]],
                                scratchSize: Int, out: SpectrumBuffers) {
        var n = 0
        var sum = 0.0, sumSq = 0.0, mean = 0.0, m2 = 0.0
        var lo = Double.infinity, hi = -Double.infinity
        var ok = scratchSize > 0
        if ok {
            let scratch = UnsafeMutablePointer<Float>.allocate(capacity: scratchSize)
            defer { scratch.deallocate() }
            for group in groups {
                guard reader.values(plane: plane, runs: group, into: scratch) else {
                    ok = false
                    break
                }
                let m = group.reduce(0) { $0 + $1.count }
                for i in 0..<m {
                    let v = scratch[i]
                    guard v.isFinite else { continue }
                    let d = Double(v)
                    n += 1
                    sum += d
                    sumSq += d * d
                    let delta = d - mean            // Welford's method, as in RegionStatistics
                    mean += delta / Double(n)
                    m2 += delta * (d - mean)
                    if d < lo { lo = d }
                    if d > hi { hi = d }
                }
            }
        }
        guard ok, n > 0 else {
            out.count[k] = 0
            for p in [out.sum, out.mean, out.stdDev, out.min, out.max, out.rms] { p[k] = .nan }
            return
        }
        out.count[k] = n
        out.sum[k] = sum
        out.mean[k] = mean
        out.stdDev[k] = n > 1 ? (m2 / Double(n - 1)).squareRoot() : 0     // sample standard deviation
        out.min[k] = lo
        out.max[k] = hi
        out.rms[k] = (sumSq / Double(n)).squareRoot()
    }
}
