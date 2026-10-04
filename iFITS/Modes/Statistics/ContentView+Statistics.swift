//
//  ContentView+Statistics.swift
//  iFITS Start
//
//  Recomputing statistics when the image or region changes.
//

import SwiftUI

extension ContentView {
    // MARK: - Statistics (S mode)

    /// Recomputes the statistics for `statsRequest` off the main thread. A short pause first lets
    /// quick region drags settle; a newer request cancels this one.
    func updateStatistics() async {
        guard let request = statsRequest else { return }
        try? await Task.sleep(for: .milliseconds(30))
        guard !Task.isCancelled else { return }
        let data = renderer.pixelData
        let pixels = data.pixels, width = data.width, height = data.height
        let region = request.region
        let stats = await Task.detached(priority: .userInitiated) {
            RegionStatistics.compute(pixels: pixels, width: width, height: height, region: region)
        }.value
        guard !Task.isCancelled else { return }
        regionStats = stats
    }
}
