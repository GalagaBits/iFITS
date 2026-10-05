//
//  ContentView+AR.swift
//  iFITS Start
//
//  Opening the AR / 3-D view of the cube from the main window.
//

import SwiftUI

extension ContentView {
    /// The AR button works for cubes (more than one channel) whose data can be read.
    var canOpenAR: Bool {
        cubeSource != nil && signalImage != nil && fitsImage != nil
    }

    /// Takes what the AR view needs (the cube, its axes, the current colormap and clip) and opens it.
    func openAR() {
        guard let cube = cubeSource, let reader = signalImage, !cube.axes.isEmpty else { return }
        // Depth = the first axis after x and y with more than one channel (usually NAXIS3).
        let depthIndex = cube.axes.firstIndex { $0.length > 1 } ?? 0
        // Other axes (e.g. Stokes) stay on the channel shown now.
        var indices = animator.indices
        if indices.count != cube.axes.count { indices = Array(repeating: 0, count: cube.axes.count) }
        let planes = (0..<cube.axes[depthIndex].length).map { k -> Int in
            var i = indices
            i[depthIndex] = k
            return cube.planeIndex(i)
        }
        let percentile: Double?
        if case .percentile(let p) = clipSelection { percentile = p } else { percentile = nil }
        animator.isPlaying = false
        arSource = ARSource(fileName: fileName, reader: reader, planes: planes,
                            depthAxis: cube.axes[depthIndex], header: headerDict, wcs: wcs,
                            colormap: renderSettings.colormap, inverted: renderSettings.inverted,
                            percentile: percentile, clipMin: renderSettings.clipMin,
                            clipMax: renderSettings.clipMax, unit: valueUnit)
    }

    /// Hosts the full-screen AR view.
    var arHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .fullScreenCover(item: $arSource, onDismiss: { restoreImageFocus() }) { source in
                ARModeView(source: source, onClose: { arSource = nil })
            }
    }
}
