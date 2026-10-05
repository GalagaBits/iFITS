//
//  ARModel.swift
//  iFITS Start
//
//  What the AR / 3-D view shows, and its settings.
//

import SwiftUI

/// Everything the AR view needs from the main window, taken when the AR button is tapped.
struct ARSource: Identifiable {
    let id = UUID()
    let fileName: String
    /// The shown image HDU, read straight from the file.
    let reader: FITSImageReader
    /// FITS plane number for each channel along the depth axis.
    let planes: [Int]
    /// The axis drawn as depth (usually NAXIS3: wavelength, frequency or velocity).
    let depthAxis: CubeAxis
    let header: [String: String]
    let wcs: WCS
    let colormap: Colormap
    let inverted: Bool
    /// The main view's clip: a percentile, or nil for its manual range (clipMin…clipMax).
    let percentile: Double?
    let clipMin: Double
    let clipMax: Double
    /// BUNIT.
    let unit: String
}

/// How solid the cube looks.
enum AROpacity: String, CaseIterable, Identifiable {
    case low, medium, high

    var id: Self { self }
    var title: String { rawValue.capitalized }

    /// How much light a box length of the top value absorbs (see arVolumeFragment).
    var density: Float {
        switch self {
        case .low: 6
        case .medium: 20
        case .high: 60
        }
    }
}

/// Renderer inputs that come from SwiftUI. A change to any of them redraws the 3-D view.
struct ARRenderSettings: Equatable {
    var colormap: Colormap
    var inverted: Bool
    var lo: Double
    var hi: Double
    var density: Float
    var showGrid: Bool
    var inRoom: Bool
    var depthStretch: Float
    var resetToken: Int
}

/// A grid label placed on screen (view points).
struct ARScreenLabel: Identifiable, Equatable {
    let id: Int
    let text: String
    let point: CGPoint
    let isTitle: Bool
}

@MainActor
@Observable
final class ARModel {
    let source: ARSource

    var volume: ARVolume?
    var axes: ARAxisSet?
    var isBuilding = true
    var progress: Double = 0
    var failure: String?

    // Settings
    var colormap: Colormap
    var inverted: Bool
    var showGrid = true
    /// Camera mode: the cube in the room.
    var inRoom = false
    var opacity: AROpacity = .medium
    /// Range of values shown: a percentile of all voxels, or nil for the main view's range.
    var percentile: Double?
    /// Length of the depth axis compared with the image's longer side.
    var depthStretch: Float = 1
    var resetToken = 0

    // From the renderer
    var labels: [ARScreenLabel] = []
    var status: String?

    static let percentiles: [Double] = [90, 95, 99, 99.5, 99.9, 99.95, 99.99, 100]

    /// The usual percentiles plus the one the main view uses (it can be a custom value).
    var percentileChoices: [Double] {
        Array(Set(Self.percentiles + [source.percentile].compactMap { $0 })).sorted()
    }

    init(source: ARSource) {
        self.source = source
        colormap = source.colormap
        inverted = source.inverted
        percentile = source.percentile
    }

    /// Values at the transparent and the most opaque end of the colorbar.
    var range: (lo: Double, hi: Double) {
        if let p = percentile, let stats = volume?.stats { return stats.clipRange(percentile: p) }
        let lo = source.clipMin, hi = source.clipMax
        return hi > lo ? (lo, hi) : (lo, lo + 1)
    }

    var settings: ARRenderSettings {
        let r = range
        return ARRenderSettings(colormap: colormap, inverted: inverted, lo: r.lo, hi: r.hi,
                                density: opacity.density, showGrid: showGrid, inRoom: inRoom,
                                depthStretch: depthStretch, resetToken: resetToken)
    }

    /// "MJy/sr", or "no unit".
    var unitText: String { source.unit.isEmpty ? "no unit" : source.unit }

    /// Reads the whole cube (every channel) off the main thread, then works out the axes.
    func load() async {
        guard volume == nil, failure == nil else { return }
        isBuilding = true
        let reader = source.reader
        let planes = source.planes
        let job = Task.detached(priority: .userInitiated) {
            ARVolume.build(reader: reader, planes: planes) { fraction in
                Task { @MainActor in self.progress = fraction }
            }
        }
        // Closing the AR view cancels this task; pass that on to the reading.
        let built = await withTaskCancellationHandler {
            await job.value
        } onCancel: {
            job.cancel()
        }
        isBuilding = false
        guard !Task.isCancelled else { return }
        guard let built else {
            failure = "Couldn't read the cube's data."
            return
        }
        axes = ARAxisTicks.make(wcs: source.wcs, header: source.header,
                                extentX: built.extentX, extentY: built.extentY, extentZ: built.extentZ,
                                depthAxis: source.depthAxis)
        volume = built
        if built.isThinned {
            var parts: [String] = []
            if built.spatialStep > 1 { parts.append("every \(built.spatialStep)\(ordinal(built.spatialStep)) pixel") }
            if built.depthStep > 1 { parts.append("every \(built.depthStep)\(ordinal(built.depthStep)) channel") }
            status = "This cube is large, so " + parts.joined(separator: " and ")
                + " is shown (raw values, nothing averaged)."
        }
    }

    private func ordinal(_ n: Int) -> String {
        switch (n % 10, n % 100) {
        case (1, let t) where t != 11: "st"
        case (2, let t) where t != 12: "nd"
        case (3, let t) where t != 13: "rd"
        default: "th"
        }
    }
}
