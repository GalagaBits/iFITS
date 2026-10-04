//
//  ImageStats.swift
//  iFITS Start
//
//  Image statistics for clipping: percentiles and histogram.
//

import SwiftUI

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
        var lo = Float.infinity, hi = -Float.infinity
        for v in values where v.isFinite {
            if v < lo { lo = v }
            if v > hi { hi = v }
        }

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
        sample.sort()
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

    static func quantile(_ s: [Float], _ q: Double) -> Double {
        guard !s.isEmpty else { return 0 }
        let pos = min(max(q, 0), 1) * Double(s.count - 1)
        let i = Int(pos)
        let f = pos - Double(i)
        let a = Double(s[i]), b = Double(s[min(i + 1, s.count - 1)])
        return a + (b - a) * f
    }

    /// Symmetric clip like CARTA: 99.9% keeps the 0.05%–99.95% range.
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
