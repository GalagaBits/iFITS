//
//  SpectrumModel.swift
//  iFITS Start
//
//  Spectra mode's settings and the latest spectrum: what it's taken over, the statistic, the
//  zoom, and the persistent spectrum box (top right).
//

import SwiftUI

@MainActor
@Observable
final class SpectrumModel {
    /// What the spectrum is taken over. "Active" (the Pixel Info pixel) to start with, like CARTA.
    var source: SpectrumSource = .active
    var statistic: SpectrumStatistic = .mean
    /// The Spectra dock is expanded.
    var expanded = true
    /// The spectrum box at the top right. It appears when you leave Spectra mode and stays (in every
    /// other mode) until closed with its ✕.
    var showBox = false

    /// The latest spectrum (kept while the next one is computed, so the graph never blinks).
    var result: SpectrumResult?
    var isComputing = false
    var progress: Double = 0
    /// Show a progress bar (bigger areas only; a single pixel is near-instant).
    var showsProgress = false

    /// Visible channel range of the graphs (fractional, 0-based; nil = every channel). Shared by the
    /// dock and the box, so zooming one zooms both.
    var zoom: ClosedRange<Double>? = nil

    /// A new image was opened.
    func imageChanged() {
        result = nil
        isComputing = false
        progress = 0
        showsProgress = false
        zoom = nil
        source = .active
        showBox = false
    }

    /// The spectrum for `token`'s image, or nil if the latest one belongs to an earlier image.
    func result(for token: UUID) -> SpectrumResult? {
        guard let result, result.key.token == token else { return nil }
        return result
    }
}
