//
//  NiceTicks.swift
//  iFITS Start
//
//  Round tick values (1, 2, 5 × 10ⁿ) and their labels, for colorbars and axes.
//

import Foundation

nonisolated enum NiceTicks {
    /// The round step (1, 2 or 5 × 10ⁿ) nearest above `x`.
    static func roundStep(_ x: Double) -> Double {
        guard x > 0, x.isFinite else { return 1 }
        let e = pow(10, floor(log10(x)))
        let f = x / e
        return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * e
    }

    /// The round step (1, 2 or 5 × 10ⁿ) nearest to `x`.
    static func nearestStep(_ x: Double) -> Double {
        guard x > 0, x.isFinite else { return 1 }
        let e = pow(10, floor(log10(x)))
        let f = x / e
        return (f < 1.5 ? 1 : f < 3.5 ? 2 : f < 7.5 ? 5 : 10) * e
    }

    /// About `target` round values between `lo` and `hi` (inclusive), and their step.
    static func values(lo: Double, hi: Double, target: Int = 5) -> (values: [Double], step: Double) {
        let a = min(lo, hi), b = max(lo, hi)
        guard a.isFinite, b.isFinite, b > a else { return (a.isFinite ? [a] : [], 1) }
        let step = nearestStep((b - a) / Double(max(1, target)))
        var out: [Double] = []
        var v = (a / step).rounded(.up) * step
        while v <= b + step * 1e-9, out.count < 100 {
            out.append(abs(v) < step * 1e-9 ? 0 : v)
            v += step
        }
        return (out, step)
    }

    /// A tick label with just enough decimals for `step`; scientific notation for very large or
    /// very small numbers.
    static func label(_ value: Double, step: Double) -> String {
        let big = max(abs(value), abs(step))
        if big >= 1e6 || (big > 0 && big < 1e-3) {
            return String(format: "%.3g", value)
        }
        let decimals = step > 0 ? min(6, max(0, Int(ceil(-log10(step) - 1e-9)))) : 0
        return String(format: "%.\(decimals)f", value)
    }
}
