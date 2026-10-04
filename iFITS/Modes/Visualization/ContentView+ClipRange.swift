//
//  ContentView+ClipRange.swift
//  iFITS Start
//
//  Applying a clip percentile or a manual clip range.
//

import SwiftUI

extension ContentView {
    // MARK: - Clip Range

    func applyPercentile(_ p: Double) {
        guard let stats = imageStats else { return }
        clipSelection = .percentile(p)
        let (lo, hi) = stats.clipRange(percentile: p)
        renderSettings.clipMin = lo
        renderSettings.clipMax = hi
    }

    func setManualClip(_ lo: Double, _ hi: Double) {
        var a = lo, b = hi
        if a > b { swap(&a, &b) }
        if a == b { b = a + max(abs(a) * 1e-6, 1e-12) }
        clipSelection = .manual
        renderSettings.clipMin = a
        renderSettings.clipMax = b
    }
}
