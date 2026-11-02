//
//  ImageStats.swift
//  iFITS Start
//
//  Image statistics for clipping: percentiles and histogram.
//

import SwiftUI
import Foundation

nonisolated struct ImageStats {
    /// Finite pixel values (sub-sampled for very large images), ascending.
    let sorted: [Float]
    let dataMin: Double
    let dataMax: Double
    /// Range shown in the histogram (central 99.99% of the data, padded).
    let histLo: Double
    let histHi: Double
    let bins: [Int]

    init(values: [Float], width: Int = 0, binCount: Int = 256, maxSamples: Int = 1_000_000) {
        // Smallest and largest finite value, on every CPU core (each chunk finds its own, then the
        // chunks' results are combined), instead of one long loop over every pixel.
        let (lo, hi) = ImageStats.finiteRange(values)

        // Evenly spread sample; the step is kept odd and off the row width to avoid column striping.
        var step = max(1, values.count / maxSamples)
        if step > 1 {
            step |= 1
            if width > 0, width % step == 0 { step += 2 }
        }
        var sample: [Float] = []
        sample.reserveCapacity(values.count / step + 1)
        var i = 0
        while i < values.count {
            let v = values[i]
            if v.isFinite { sample.append(v) }
            i += step
        }
        // Sorted on every CPU core (same result as sample.sort(), several times faster). This is
        // the slowest step of reading a cube channel, so it matters when stepping through channels.
        sample = ImageStats.parallelSorted(sample)
        sorted = sample

        guard lo.isFinite, hi.isFinite, !sample.isEmpty else {
            dataMin = 0; dataMax = 1; histLo = 0; histHi = 1
            bins = Array(repeating: 0, count: binCount)
            return
        }
        dataMin = Double(lo)
        dataMax = Double(hi)

        let qLo = ImageStats.quantile(sample, 0.0001)
        let qHi = ImageStats.quantile(sample, 0.9999)
        let pad = (qHi - qLo) * 0.05
        var a = max(Double(lo), qLo - pad)
        var b = min(Double(hi), qHi + pad)
        if !(b > a) {
            a = Double(lo)
            b = Double(hi) > Double(lo) ? Double(hi) : Double(lo) + 1
        }
        histLo = a
        histHi = b

        var counts = [Int](repeating: 0, count: binCount)
        let k = Double(binCount) / (b - a)
        for v in sample {
            let d = Double(v)
            if d < a || d > b { continue }
            counts[min(binCount - 1, Int((d - a) * k))] += 1
        }
        bins = counts
    }

    /// The values sorted ascending, using every core: each core sorts its own part, then the
    /// sorted parts are merged pairwise (also in parallel) until one sorted array is left.
    /// Gives exactly the same order as `sorted()` (only finite values are passed in).
    static func parallelSorted(_ values: [Float]) -> [Float] {
        let n = values.count
        let cores = ProcessInfo.processInfo.activeProcessorCount
        guard n >= 131_072, cores > 1 else { return values.sorted() }
        // Part p covers bounds[p]..<bounds[p + 1].
        let count = min(16, cores)
        var bounds = (0...count).map { $0 * n / count }
        var a = values
        var b = [Float](repeating: 0, count: n)
        a.withUnsafeMutableBufferPointer { aBuf in
            b.withUnsafeMutableBufferPointer { bBuf in
                guard let aBase = aBuf.baseAddress, let bBase = bBuf.baseAddress else { return }
                let job = SortJob(src: aBase, dst: bBase)
                // 1. Sort each part.
                let firstBounds = bounds
                DispatchQueue.concurrentPerform(iterations: count) { p in
                    job.sortRange(firstBounds[p], firstBounds[p + 1])
                }
                // 2. Merge neighbouring parts until one is left, swapping source and destination
                //    each round.
                var from = job.src, to = job.dst
                while bounds.count > 2 {
                    let current = bounds
                    let pairs = current.count / 2      // parts = current.count − 1, rounded up / 2
                    let step = MergeJob(src: from, dst: to)
                    DispatchQueue.concurrentPerform(iterations: pairs) { k in
                        let lo = current[2 * k]
                        let mid = current[min(2 * k + 1, current.count - 1)]
                        let hi = current[min(2 * k + 2, current.count - 1)]
                        step.merge(lo, mid, hi)
                    }
                    bounds = stride(from: 0, to: current.count, by: 2).map { current[$0] }
                    if bounds.last != n { bounds.append(n) }
                    swap(&from, &to)
                }
                // The result is in `from`; make sure it ends up in `a`.
                if from != aBase { aBase.update(from: from, count: n) }
            }
        }
        return a
    }

    /// (min, max) of the finite values; (inf, -inf) when there are none.
    static func finiteRange(_ values: [Float]) -> (Float, Float) {
        let n = values.count
        let chunks = n < 262_144 ? 1 : max(1, ProcessInfo.processInfo.activeProcessorCount * 2)
        var los = [Float](repeating: .infinity, count: chunks)
        var his = [Float](repeating: -.infinity, count: chunks)
        values.withUnsafeBufferPointer { src in
            los.withUnsafeMutableBufferPointer { loBuf in
                his.withUnsafeMutableBufferPointer { hiBuf in
                    guard let s = src.baseAddress, let l = loBuf.baseAddress, let h = hiBuf.baseAddress else { return }
                    let job = RangeJob(src: s, los: l, his: h)
                    DispatchQueue.concurrentPerform(iterations: chunks) { c in
                        job.run(chunk: c, from: c * n / chunks, to: (c + 1) * n / chunks)
                    }
                }
            }
        }
        return (los.min() ?? .infinity, his.max() ?? -.infinity)
    }

    static func quantile(_ s: [Float], _ q: Double) -> Double {
        guard !s.isEmpty else { return 0 }
        let pos = min(max(q, 0), 1) * Double(s.count - 1)
        let i = Int(pos)
        let f = pos - Double(i)
        let a = Double(s[i]), b = Double(s[min(i + 1, s.count - 1)])
        return a + (b - a) * f
    }

    /// Clip range that keeps the central p% of pixels: 99.9% keeps 0.05%–99.95%
    func clipRange(percentile p: Double) -> (Double, Double) {
        guard !sorted.isEmpty else { return (0, 1) }
        if p >= 100 {
            return dataMax > dataMin ? (dataMin, dataMax) : (dataMin, dataMin + 1)
        }
        let tail = (100 - max(0, p)) / 200
        let a = ImageStats.quantile(sorted, tail)
        let b = ImageStats.quantile(sorted, 1 - tail)
        return b > a ? (a, b) : (a, a + max(abs(a) * 1e-6, 1e-12))
    }
}

/// One chunk of ImageStats.finiteRange: finds its min and max and stores them in its own slot.
nonisolated private struct RangeJob: @unchecked Sendable {
    let src: UnsafePointer<Float>
    let los: UnsafeMutablePointer<Float>
    let his: UnsafeMutablePointer<Float>

    func run(chunk c: Int, from start: Int, to end: Int) {
        var lo = Float.infinity, hi = -Float.infinity
        for i in start..<end {
            let v = src[i]
            if v.isFinite {
                if v < lo { lo = v }
                if v > hi { hi = v }
            }
        }
        los[c] = lo
        his[c] = hi
    }
}

/// Sorting one part of ImageStats.parallelSorted (each part is touched by one thread only).
nonisolated private struct SortJob: @unchecked Sendable {
    let src: UnsafeMutablePointer<Float>
    let dst: UnsafeMutablePointer<Float>

    func sortRange(_ lo: Int, _ hi: Int) {
        guard hi - lo > 1 else { return }
        var part = UnsafeMutableBufferPointer(start: src + lo, count: hi - lo)
        part.sort()
    }
}

/// Merging two neighbouring sorted parts [lo, mid) and [mid, hi) from `src` into `dst`.
nonisolated private struct MergeJob: @unchecked Sendable {
    let src: UnsafeMutablePointer<Float>
    let dst: UnsafeMutablePointer<Float>

    func merge(_ lo: Int, _ mid: Int, _ hi: Int) {
        var i = lo, j = mid, k = lo
        while i < mid && j < hi {
            if src[j] < src[i] { dst[k] = src[j]; j += 1 } else { dst[k] = src[i]; i += 1 }
            k += 1
        }
        while i < mid { dst[k] = src[i]; i += 1; k += 1 }
        while j < hi { dst[k] = src[j]; j += 1; k += 1 }
    }
}
