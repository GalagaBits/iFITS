//
//  ContentView+Cube.swift
//  iFITS Start
//
//  Cube mode: switching channels and showing planes.
//

import SwiftUI

extension ContentView {
    // MARK: - Cubes (C mode)

    /// The cube button (top right): turns cube mode on, or back off to the previous mode.
    func toggleCubeMode() {
        guard cubeSource != nil else { return }
        if selectedMode == "C" {
            selectMode(modeBeforeCube)
        } else {
            // Remember the V A R S mode (not Spectra) to come back to.
            if modes.contains(selectedMode) { modeBeforeCube = selectedMode }
            animatorExpanded = true
            selectMode("C")
        }
    }

    /// Asks for the plane of the current channel(s), if it isn't the one already asked for.
    func showCurrentPlane() {
        guard let cube = cubeSource else { return }
        let plane = cube.planeIndex(animator.indices)
        guard plane != requestedPlane else { return }
        requestedPlane = plane
        var percentile: Double? = nil
        if case .percentile(let p) = clipSelection { percentile = p }
        // With the GPU view, the plane loader skips drawing a CPU picture (the GPU colours it).
        planeLoader.request(CubePlaneRequest(source: cube, plane: plane, settings: renderSettings,
                                             percentile: percentile, drawImage: !gpuImage.isReady))
    }

    /// A new plane is ready: show it. With a clip percentile, each channel gets its own clip
    /// range (like CARTA's per-channel histogram); a manual clip is kept.
    func applyPlane(_ result: CubePlaneResult) {
        guard let cube = cubeSource else { return }
        // Keep any render changes made while the plane was loading (colormap, scaling, …).
        var settings = renderSettings
        if case .percentile(let p) = clipSelection {
            (settings.clipMin, settings.clipMax) = result.stats.clipRange(percentile: p)
        }
        renderer.load(result.pixels, width: cube.width, height: cube.height, renderedWith: result.settings)
        // The new channel's pixels go to the GPU (one upload per channel).
        gpuImage.load(pixels: result.pixels, width: cube.width, height: cube.height)
        imageStats = result.stats
        if let image = result.image { fitsImage = image }
        imageGeneration += 1          // statistics follow the channel
        if settings != renderSettings {
            renderSettings = settings  // the GPU view redraws; without it, onChange re-renders
        }
        // Without the GPU view: draw the CPU picture if it wasn't drawn, or drawn with other settings.
        if !gpuImage.isReady, result.image == nil || settings != result.settings {
            renderer.refresh(settings)
        }
    }

    /// Commands from the menu bar's Cube menu.
    func cubeCommand(_ command: CubeCommand) {
        guard cubeSource != nil, fitsImage != nil else { return }
        switch command {
        case .toggleMode: toggleCubeMode()
        case .toggleSpectra: toggleSpectraMode()
        case .playPause: animator.togglePlay()
        case .next: animator.step(1)
        case .previous: animator.step(-1)
        case .first: animator.first()
        case .last: animator.last()
        case .showMiniAnimator:
            withAnimation(.snappy) {
                showMiniAnimator = true
                // It lives beside the collapsed animator, so collapse it if it's open.
                if selectedMode == "C" { animatorExpanded = false }
            }
        }
    }
}
