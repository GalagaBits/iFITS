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
        // With the GPU view, the plane loader skips drawing a CPU picture (the GPU colours it) and
        // copies the plane to the GPU in the background. It also loads upcoming channels ahead
        // (about two seconds of frames while playing, otherwise the next channel).
        planeLoader.request(planeRequest(cube, plane: plane))
    }

    /// A request for `plane` with the current settings, plus the planes to load ahead.
    func planeRequest(_ cube: CubeSource, plane: Int) -> CubePlaneRequest {
        var percentile: Double? = nil
        if case .percentile(let p) = clipSelection { percentile = p }
        return CubePlaneRequest(source: cube, plane: plane, settings: renderSettings,
                                percentile: percentile, drawImage: !gpuImage.isReady,
                                gpu: gpuImage.isReady ? gpuImage.uploader : nil,
                                readAhead: upcomingPlanes(cube))
    }

    /// Planes to load ahead: while playing, about two seconds of upcoming frames (like a video
    /// player's buffer, limited by memory in the loader); otherwise just the next channel.
    func upcomingPlanes(_ cube: CubeSource) -> [Int] {
        let frames = animator.isPlaying ? min(120, max(4, animator.framesPerSecond * 2)) : 1
        return animator.upcomingIndices(frames).map { cube.planeIndex($0) }
    }

    /// Playback, like a video player. The buffer starts filling as soon as Play is pressed, and
    /// each frame moves on at the chosen frame rate only once the next channel is loaded. If it
    /// isn't ready yet, playback waits a moment (buffering) instead of jumping ahead and stuttering.
    func runPlayback() async {
        guard playbackKey.playing, let cube = cubeSource else { return }
        let request = planeRequest(cube, plane: cube.planeIndex(animator.indices))
        planeLoader.setReadAhead(request.readAhead, template: request)
        let clock = ContinuousClock()
        let interval = Duration.seconds(1.0 / Double(max(1, playbackKey.framesPerSecond)))
        var due = clock.now + interval
        while !Task.isCancelled {
            try? await clock.sleep(until: due)
            if Task.isCancelled { break }
            guard let source = cubeSource else { break }
            let next = animator.upcomingIndices(1).first.map { source.planeIndex($0) }
            if next.map({ planeLoader.readyToShow($0) }) ?? true {
                animator.tick()                     // shows it (from the cache) and slides the buffer
                due = due + interval
                if due < clock.now { due = clock.now + interval }   // was buffering: restart the beat
            } else {
                due = clock.now + .milliseconds(15) // not loaded yet: check again shortly
            }
        }
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
        // The new channel is already on the GPU (uploaded in the background); otherwise upload it now.
        if let prepared = result.gpuImage {
            gpuImage.install(prepared)
        } else {
            gpuImage.load(pixels: result.pixels, width: cube.width, height: cube.height)
        }
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
