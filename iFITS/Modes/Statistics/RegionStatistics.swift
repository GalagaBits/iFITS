//
//  RegionStatistics.swift
//  iFITS Start
//
//  Statistics (S): pixel count, sum, mean, std, min, max, RMS.
//

import SwiftUI

/// Statistics of the pixels inside a region, like CARTA's Statistics widget.
nonisolated struct RegionStatistics: Equatable, Sendable {
    var count = 0
    var sum = Double.nan
    var mean = Double.nan
    var stdDev = Double.nan
    var min = Double.nan
    var max = Double.nan
    var rms = Double.nan

    /// Statistics of the finite pixels whose centers are inside `region` (the whole image if nil).
    /// `pixels` are in display order: row 0 is the top row (FITS row = height).
    /// Returns nil for lines, which have no area.
    static func compute(pixels: [Float], width: Int, height: Int, region: FITSRegion?) -> RegionStatistics? {
        guard width > 0, height > 0, pixels.count >= width * height else { return nil }
        if let region, !region.shape.hasStatistics { return nil }

        // Double → pixel number, safe for huge or non-finite values.
        func pixelIndex(_ v: Double, _ rule: FloatingPointRoundingRule) -> Int {
            guard v.isFinite else { return v > 0 ? Int.max / 2 : Int.min / 2 }
            return Int(Swift.min(Swift.max(v, -1e9), 1e9).rounded(rule))
        }

        // FITS pixel ranges to scan (inclusive), and the shape test.
        var x0 = 1, x1 = width, y0 = 1, y1 = height
        var cx = 0.0, cy = 0.0, cosA = 1.0, sinA = 0.0, hw = 0.0, hh = 0.0
        var test = 0          // 0 = every pixel, 1 = rectangle, 2 = ellipse
        if let region {
            cx = Double(region.center.x)
            cy = Double(region.center.y)
            switch region.shape {
            case .point:
                x0 = pixelIndex(cx, .toNearestOrAwayFromZero); x1 = x0
                y0 = pixelIndex(cy, .toNearestOrAwayFromZero); y1 = y0
            case .ellipse, .rectangle:
                cosA = cos(region.radians); sinA = sin(region.radians)
                hw = Double(region.size.width) / 2
                hh = Double(region.size.height) / 2
                let ex = abs(hw * cosA) + abs(hh * sinA)
                let ey = abs(hw * sinA) + abs(hh * cosA)
                x0 = pixelIndex(cx - ex, .up); x1 = pixelIndex(cx + ex, .down)
                y0 = pixelIndex(cy - ey, .up); y1 = pixelIndex(cy + ey, .down)
                test = region.shape == .ellipse ? 2 : 1
            case .line:
                return nil
            }
            x0 = Swift.max(x0, 1); x1 = Swift.min(x1, width)
            y0 = Swift.max(y0, 1); y1 = Swift.min(y1, height)
            guard cx.isFinite, cy.isFinite, hw.isFinite, hh.isFinite else { return RegionStatistics() }
        }

        var n = 0
        var sum = 0.0, sumSq = 0.0, mean = 0.0, m2 = 0.0
        var lo = Double.infinity, hi = -Double.infinity

        if x0 <= x1, y0 <= y1 {
            pixels.withUnsafeBufferPointer { buf in
                for fy in y0...y1 {
                    let rowStart = (height - fy) * width - 1     // + fx = index of FITS pixel (fx, fy)
                    let dy = Double(fy) - cy
                    for fx in x0...x1 {
                        if test != 0 {
                            let dx = Double(fx) - cx
                            let lx = dx * cosA + dy * sinA
                            let ly = -dx * sinA + dy * cosA
                            if test == 1 {
                                if abs(lx) > hw || abs(ly) > hh { continue }
                            } else {
                                guard hw > 0, hh > 0 else { continue }
                                if (lx * lx) / (hw * hw) + (ly * ly) / (hh * hh) > 1 { continue }
                            }
                        }
                        let v = buf[rowStart + fx]
                        guard v.isFinite else { continue }
                        let d = Double(v)
                        n += 1
                        sum += d
                        sumSq += d * d
                        let delta = d - mean            // Welford's method: a stable standard deviation
                        mean += delta / Double(n)
                        m2 += delta * (d - mean)
                        if d < lo { lo = d }
                        if d > hi { hi = d }
                    }
                }
            }
        }

        var r = RegionStatistics()
        r.count = n
        guard n > 0 else { return r }
        r.sum = sum
        r.mean = mean
        r.stdDev = n > 1 ? (m2 / Double(n - 1)).squareRoot() : 0      // sample standard deviation, like CARTA
        r.min = lo
        r.max = hi
        r.rms = (sumSq / Double(n)).squareRoot()
        return r
    }
}
