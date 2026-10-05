//
//  SpectrumModel.swift
//  iFITS Start
//
//  Spectra mode's settings and the latest spectra: what they're taken over (up to 10 overplotted),
//  the statistic, the zoom, and the persistent spectrum box (top right).
//

import SwiftUI

/// Line colours, in order: Matplotlib's default "tab10" cycle (blue, orange, green, red, …).
enum SpectrumPalette {
    static let hex: [UInt32] = [0x1f77b4, 0xff7f0e, 0x2ca02c, 0xd62728, 0x9467bd,
                                0x8c564b, 0xe377c2, 0x7f7f7f, 0xbcbd22, 0x17becf]

    static func color(_ index: Int) -> Color {
        let v = hex[((index % hex.count) + hex.count) % hex.count]
        return Color(red: Double((v >> 16) & 0xFF) / 255,
                     green: Double((v >> 8) & 0xFF) / 255,
                     blue: Double(v & 0xFF) / 255)
    }
}

@MainActor
@Observable
final class SpectrumModel {
    /// Most spectra overplotted at once (one per tab10 colour).
    static let maxSources = 10

    /// What the spectra are taken over, in the order chosen (that order picks the colours).
    /// "Active" (the Pixel Info pixel) to start with, like CARTA. Never empty.
    private(set) var sources: [SpectrumSource] = [.active]
    var statistic: SpectrumStatistic = .mean
    /// The Spectra dock is expanded.
    var expanded = true
    /// The spectrum box at the top right. It appears when you leave Spectra mode and stays (in every
    /// other mode) until closed with its ✕.
    var showBox = false

    /// The latest spectrum for each source (kept while the next one is computed, so the graph
    /// never blinks).
    private(set) var results: [SpectrumResult] = []
    var isComputing = false
    var progress: Double = 0
    /// Show a progress bar (bigger areas only; a single pixel is near-instant).
    var showsProgress = false

    /// Visible channel range of the graphs (fractional, 0-based; nil = every channel). Shared by every
    /// graph (dock, box, window), so zooming one zooms them all.
    var zoom: ClosedRange<Double>? = nil

    /// Shows just this one (a tap on a region or on the image).
    func selectOnly(_ source: SpectrumSource) {
        if sources != [source] { sources = [source] }
    }

    /// Adds or removes a spectrum (the menu). The last one can't be removed; at most 10.
    func toggle(_ source: SpectrumSource) {
        if let i = sources.firstIndex(of: source) {
            guard sources.count > 1 else { return }
            sources.remove(at: i)
        } else if sources.count < Self.maxSources {
            sources.append(source)
        }
    }

    /// Drops deleted regions (and lines) from the spectra; Active if nothing is left.
    func keepOnly(regions ids: Set<UUID>) {
        let kept = sources.filter { source in
            if case .region(let id) = source { return ids.contains(id) }
            return true
        }
        if kept != sources { sources = kept.isEmpty ? [.active] : kept }
    }

    /// Keeps a new result, replacing the earlier one for the same source.
    func store(_ result: SpectrumResult) {
        // A new number of channels (another file) starts zoomed out.
        if let old = results.first, old.channelCount != result.channelCount { zoom = nil }
        results.removeAll { $0.key.source == result.key.source || !sources.contains($0.key.source) }
        results.append(result)
    }

    /// The result for `key`, or (while that one is computed) the last one for the same source of
    /// the same image and channels.
    func result(for key: SpectrumKey) -> SpectrumResult? {
        results.first { $0.key == key }
            ?? results.first { $0.key.source == key.source && $0.key.token == key.token && $0.key.planes == key.planes }
    }

    /// A new image was opened.
    func imageChanged() {
        results = []
        isComputing = false
        progress = 0
        showsProgress = false
        zoom = nil
        sources = [.active]
        showBox = false
    }
}
