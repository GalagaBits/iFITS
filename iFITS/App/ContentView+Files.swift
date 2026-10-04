//
//  ContentView+Files.swift
//  iFITS Start
//
//  Opening, loading, saving and renaming FITS files.
//

import SwiftUI
import PencilKit

extension ContentView {
    // MARK: - Saving annotations and regions into the FITS file

    /// Default save ("Save", ⌘S): writes the annotations (APPLE_PENCIL_ANNOTATIONS) and regions
    /// (DS9_REGIONS) into the currently loaded FITS file, replacing any earlier ones.
    func saveToOriginal() {
        guard let url = loadedFileURL else { return }
        let drawing = annotations.drawingData()
        let regionLines = DS9Regions.fitsLines(regionStore.regions)
        let name = fileName
        DispatchQueue.global(qos: .userInitiated).async {
            let accessing = url.startAccessingSecurityScopedResource()
            defer { if accessing { url.stopAccessingSecurityScopedResource() } }

            var failure: Error?
            var changed = false
            var coordinatorError: NSError?
            NSFileCoordinator().coordinate(writingItemAt: url, options: .forReplacing,
                                           error: &coordinatorError) { writeURL in
                do {
                    let original = try Data(contentsOf: writeURL)
                    let updated = FITSAnnotationStore.updating(original, drawing: drawing, regionLines: regionLines)
                    if updated != original {
                        try updated.write(to: writeURL, options: .atomic)
                        changed = true
                    }
                } catch {
                    failure = error
                }
            }
            let error = failure ?? coordinatorError
            let didChange = changed
            DispatchQueue.main.async {
                if let error {
                    saveError = error.localizedDescription
                } else if !didChange {
                    showSaveMessage("Nothing new to save")
                } else {
                    showSaveMessage("Saved \(name)")
                }
            }
        }
    }

    /// "Save as Copy…" (⇧⌘S): a copy of the loaded FITS file with the annotations and regions
    /// added, saved wherever you choose.
    func saveCopy() {
        guard let url = loadedFileURL else { return }
        let drawing = annotations.drawingData()
        let regionLines = DS9Regions.fitsLines(regionStore.regions)
        DispatchQueue.global(qos: .userInitiated).async {
            let accessing = url.startAccessingSecurityScopedResource()
            defer { if accessing { url.stopAccessingSecurityScopedResource() } }
            do {
                let original = try Data(contentsOf: url)
                let updated = FITSAnnotationStore.updating(original, drawing: drawing, regionLines: regionLines)
                DispatchQueue.main.async {
                    copyDocument = FITSFileDocument(data: updated)
                    showCopyExporter = true
                }
            } catch {
                DispatchQueue.main.async { saveError = error.localizedDescription }
            }
        }
    }

    // MARK: - Opening a FITS file

    func openFITSPicker() {
        importKind = .fits
        showPicker = true
    }

    // MARK: - Renaming the file (tap the title)

    /// Renames the loaded file on disk. The extension is kept if you leave it off. A bookmark made
    /// before the rename finds the file again afterwards, so Save keeps working.
    func renameLoadedFile(to proposed: String) {
        guard let url = loadedFileURL else { return }
        var name = proposed.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !name.isEmpty, name != fileName else { return }
        guard !name.contains("/"), !name.contains(":"), !name.hasPrefix(".") else {
            saveError = "A file name can't contain “/” or “:”, or start with “.”."
            return
        }
        let ext = (fileName as NSString).pathExtension
        if !ext.isEmpty, (name as NSString).pathExtension.lowercased() != ext.lowercased() {
            name += "." + ext
        }
        let newName = name
        guard newName != fileName else { return }

        DispatchQueue.global(qos: .userInitiated).async {
            let accessing = url.startAccessingSecurityScopedResource()
            defer { if accessing { url.stopAccessingSecurityScopedResource() } }
            do {
                let bookmark = try url.bookmarkData()
                var failure: Error?
                var coordinatorError: NSError?
                NSFileCoordinator().coordinate(writingItemAt: url, options: .forMoving,
                                               error: &coordinatorError) { movingURL in
                    do {
                        var values = URLResourceValues()
                        values.name = newName
                        var target = movingURL
                        try target.setResourceValues(values)
                    } catch {
                        failure = error
                    }
                }
                if let error = failure ?? coordinatorError { throw error }
                var stale = false
                let renamed = (try? URL(resolvingBookmarkData: bookmark, bookmarkDataIsStale: &stale))
                    ?? url.deletingLastPathComponent().appendingPathComponent(newName)
                DispatchQueue.main.async {
                    loadedFileURL = renamed
                    fileName = renamed.lastPathComponent
                    showSaveMessage("Renamed to \(renamed.lastPathComponent)")
                }
            } catch {
                let message = error.localizedDescription
                DispatchQueue.main.async {
                    saveError = "Couldn't rename the file: \(message)"
                }
            }
        }
    }

    // MARK: - Save message

    func showSaveMessage(_ message: String) {
        withAnimation(.snappy) { saveMessage = message }
        Task { @MainActor in
            try? await Task.sleep(for: .seconds(2))
            if saveMessage == message {
                withAnimation(.snappy) { saveMessage = nil }
            }
        }
    }

    // MARK: - Loading a FITS file

    func loadFITSFile(url: URL) {
        isLoading = true
        errorMessage = nil

        guard url.startAccessingSecurityScopedResource() else {
            errorMessage = "Permission denied."
            isLoading = false
            return
        }

        // Keep the current scaling/colormap/percentile when opening a new file.
        let baseSettings = renderSettings
        let percentile: Double
        if case .percentile(let p) = clipSelection { percentile = p } else { percentile = 99.9 }

        DispatchQueue.global(qos: .userInitiated).async {
            do {
                // Memory-mapped when possible: cube planes are read only when shown.
                let data = try Data(contentsOf: url, options: .mappedIfSafe)
                // Annotations and regions saved by iFITS (APPLE_PENCIL_ANNOTATIONS and
                // DS9_REGIONS extensions), if any.
                let savedAnnotations = FITSAnnotationStore.readDrawingData(from: data)
                let savedRegions = FITSAnnotationStore.readRegionText(from: data)
                let result = try FITSDecoder.loadFITS(with: data)
                // Physical values = BZERO + BSCALE × stored value; BLANK (integer images) → NaN.
                // Flipped so FITS row 1 is at the bottom, like CARTA / DS9 (north up, RA along x).
                let pixels = FITSPhysical.flipRows(
                    FITSPhysical.apply(result.floats, bscale: result.bscale, bzero: result.bzero,
                                       header: result.headerDict),
                    width: result.width, height: result.height)
                let stats = ImageStats(values: pixels, width: result.width)
                var settings = baseSettings
                (settings.clipMin, settings.clipMax) = stats.clipRange(percentile: percentile)
                let generatedImage = FITSRenderer.render(pixels, width: result.width,
                                                         height: result.height, settings: settings)
                // NAXIS ≥ 3 with more than one plane: a cube.
                let cube = CubeSource(data: data, width: result.width, height: result.height,
                                      bitpix: result.bitpix, dataOffset: result.dataOffset,
                                      bscale: result.bscale, bzero: result.bzero,
                                      axisLengths: result.axisLengths, header: result.headerDict)

                DispatchQueue.main.async {
                    self.renderer.load(pixels, width: result.width, height: result.height,
                                       renderedWith: settings)
                    self.imageStats = stats
                    self.clipSelection = .percentile(percentile)
                    self.renderSettings = settings
                    self.fitsImage = generatedImage
                    // The image, panel and annotation layers were just added: make sure the
                    // window still has keyboard focus.
                    self.restoreImageFocus()
                    self.annotations.removeAll()   // annotations belong to the previous image
                    if let savedAnnotations, let drawing = try? PKDrawing(data: savedAnnotations) {
                        self.annotations.restore(drawing.strokes)
                    }
                    self.loadedFileURL = url
                    self.inspectedPixel = nil
                    self.headerDict = result.headerDict
                    self.headerCards = result.headerCards
                    self.fileName = url.lastPathComponent
                    let newWCS = WCS(header: result.headerDict)
                    self.wcs = newWCS
                    // Regions belong to the previous image too; bring back any saved in this file.
                    self.regionDrag = nil
                    self.regionStore.removeAll()
                    if let savedRegions {
                        self.regionStore.append(DS9Regions.parse(savedRegions, wcs: newWCS,
                                                                 header: result.headerDict).regions)
                    }
                    self.regionStats = nil
                    self.imageGeneration += 1

                    // Cube (or not): reset the animator to channel 0.
                    self.planeLoader.reset()
                    self.requestedPlane = 0
                    self.cubeSource = cube
                    self.animator.configure(cube?.axes ?? [])
                    if cube == nil {
                        self.showMiniAnimator = false
                        if self.selectedMode == "C" { self.selectMode(self.modeBeforeCube) }
                    }

                    // Keep reading access to this file while it's open (cube planes are read
                    // from it later); give up access to the previous file.
                    if let previous = self.scopedURL { previous.stopAccessingSecurityScopedResource() }
                    self.scopedURL = url
                    self.imageWidth = CGFloat(result.width)
                    self.imageHeight = CGFloat(result.height)

                    // Reset zoom / pan
                    self.scale = 1.0
                    self.offset = .zero

                    self.isLoading = false
                    if generatedImage == nil {
                        self.errorMessage = "Failed to generate image from FITS data."
                    }
                }
            } catch {
                DispatchQueue.main.async {
                    url.stopAccessingSecurityScopedResource()
                    self.errorMessage = error.localizedDescription
                    self.isLoading = false
                }
            }
        }
    }
}
