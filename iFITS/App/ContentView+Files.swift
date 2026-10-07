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
    /// - silent: autosave. No "Saved" message, and a problem is reported once per file.
    func saveToOriginal(silent: Bool = false) {
        guard let url = loadedFileURL else { return }
        // What to save is taken now, so a save that has to wait still saves this file's edits.
        let editsSaved = edits.editCount
        let drawing = annotations.drawingData()
        let regionLines = DS9Regions.fitsLines(regionStore.regions)
        let name = fileName
        let save = {
            performSave(url: url, drawing: drawing, regionLines: regionLines, name: name,
                        editsSaved: editsSaved, silent: silent)
        }
        // One save at a time; the newest one asked for meanwhile runs right after.
        if isSaving {
            queuedSave = save
        } else {
            save()
        }
    }

    /// Writes the annotations and regions into `url` (off the main thread).
    private func performSave(url: URL, drawing: Data?, regionLines: [String], name: String,
                             editsSaved: Int, silent: Bool) {
        isSaving = true
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
                isSaving = false
                if let error {
                    if !silent || autosaveFailedURL != url {
                        saveError = (silent ? "Autosave couldn't save \(name): " : "") + error.localizedDescription
                    }
                    if silent { autosaveFailedURL = url }
                } else {
                    // Only if the same file is still open (another one may have been opened since).
                    if loadedFileURL == url { savedEditCount = editsSaved }
                    if autosaveFailedURL == url { autosaveFailedURL = nil }
                    if !silent { showSaveMessage(didChange ? "Saved \(name)" : "Nothing new to save") }
                }
                if let next = queuedSave {
                    queuedSave = nil
                    next()
                }
            }
        }
    }

    /// Edits since the last save (annotations and regions).
    var hasUnsavedEdits: Bool { edits.editCount != savedEditCount }

    /// Autosave: saves now if autosave is on and something changed.
    func autosaveNow() {
        guard autosaveEnabled, hasUnsavedEdits, let url = loadedFileURL, autosaveFailedURL != url else { return }
        saveToOriginal(silent: true)
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

    /// Open File (⌘O): asks about unsaved changes first, then shows the file picker.
    func openFITSPicker() {
        confirmLeavingFile {
            importKind = .fits
            showPicker = true
        }
    }

    /// A FITS file from the Files app (Open With, or tapping it) or dropped on the window: asks
    /// about unsaved changes first, then opens it.
    func prepareToOpen(url: URL) {
        confirmLeavingFile {
            scanAndOpen(url: url)
        }
    }

    /// Opens a FITS file. With more than one image HDU (e.g. JWST's SCI, ERR, DQ, WMAP), asks
    /// which one to show; with one, opens it straight away.
    func scanAndOpen(url: URL) {
        let name = url.lastPathComponent
        DispatchQueue.global(qos: .userInitiated).async {
            let scoped = url.startAccessingSecurityScopedResource()
            defer { if scoped { url.stopAccessingSecurityScopedResource() } }
            do {
                // A coordinated read downloads a file that's only in iCloud Drive (or another
                // cloud provider) first, so it can be opened from Files or dropped in place.
                var readError: Error?
                var coordinatorError: NSError?
                var scanned: [FITSHDUInfo] = []
                NSFileCoordinator().coordinate(readingItemAt: url, options: [], error: &coordinatorError) { readURL in
                    do {
                        let data = try Data(contentsOf: readURL, options: .mappedIfSafe)
                        scanned = FITSHDUList.scan(data)
                    } catch {
                        readError = error
                    }
                }
                if let error = readError ?? coordinatorError { throw error }
                let images = scanned.filter(\.isImage)
                DispatchQueue.main.async {
                    if images.isEmpty {
                        saveError = "\(name) has no image to show. (Its HDUs are tables or empty.)"
                    } else if images.count == 1 {
                        loadFITSFile(url: url, hdu: images[0].index)
                    } else {
                        hduPicker = HDUPickerRequest(purpose: .open, url: url, hdus: images)
                    }
                }
            } catch {
                let message = error.localizedDescription
                DispatchQueue.main.async { saveError = "Couldn't open \(name): \(message)" }
            }
        }
    }

    /// The HDU picker's choice.
    func choseHDU(_ hdu: FITSHDUInfo, for request: HDUPickerRequest) {
        hduPicker = nil
        switch request.purpose {
        case .open: loadFITSFile(url: request.url, hdu: hdu.index)
        case .noise: attachNoiseFile(url: request.url, hdu: hdu)
        }
    }

    // MARK: - Unsaved changes

    /// Runs `action` (which opens another file) once the open file's annotations and regions are
    /// safe. With autosave on, they're saved automatically as the new file loads. With autosave off
    /// (or after an autosave problem), asks first: Save, Don't Save, or Cancel.
    func confirmLeavingFile(then action: @escaping () -> Void) {
        guard let url = loadedFileURL, hasUnsavedEdits else {
            action()
            return
        }
        if autosaveEnabled, autosaveFailedURL != url {
            // Save now and open the other file once the save is done (it may be this same file,
            // opened again from Files).
            autosaveNow()
            waitForSave(then: action)
            return
        }
        pendingOpen = PendingOpen(action: action)
    }

    /// "Save": saves, then opens the other file (stays here if the save fails; the problem is shown).
    func saveThenContinue(_ pending: PendingOpen) {
        pendingOpen = nil
        saveToOriginal()
        waitForSave {
            guard !hasUnsavedEdits else { return }
            continueAfterPrompt(pending)
        }
    }

    /// Runs the waiting action once the alert has gone (a file picker can't open while it's closing).
    func continueAfterPrompt(_ pending: PendingOpen) {
        pendingOpen = nil
        Task { @MainActor in
            try? await Task.sleep(for: .milliseconds(350))
            pending.action()
        }
    }

    /// The "Save changes?" alert, on its own empty view (SwiftUI can mix up alerts that share one).
    var unsavedChangesHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .alert("Save changes to “\(fileName)”?",
                   isPresented: Binding(get: { pendingOpen != nil }, set: { if !$0 { pendingOpen = nil } }),
                   presenting: pendingOpen) { pending in
                Button("Save") { saveThenContinue(pending) }
                Button("Don't Save", role: .destructive) { continueAfterPrompt(pending) }
                Button("Cancel", role: .cancel) {
                    pendingOpen = nil
                    restoreImageFocus()
                }
            } message: { _ in
                Text("Your annotations and regions haven't been saved. If you don't save them, they'll be lost.")
            }
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

    /// Loads HDU `hdu` of the file (nil = the first image HDU).
    /// keepView: keep the zoom, position and cube channel (used when the "_SNR_" file replaces the
    /// image it was made from); a problem then shows as an alert and the current image stays.
    func loadFITSFile(url: URL, hdu: Int? = nil, keepView: Bool = false) {
        // Save the open file's edits before it's replaced.
        autosaveNow()
        isLoading = true
        errorMessage = nil

        // Files from the file picker need security-scoped access; files in the app's own
        // folders don't (and return false here), so a false isn't an error by itself.
        let scoped = url.startAccessingSecurityScopedResource()

        // Keep the current scaling/colormap/percentile when opening a new file.
        let baseSettings = renderSettings
        let percentile: Double
        if case .percentile(let p) = clipSelection { percentile = p } else { percentile = 99.9 }
        // The channel on screen, to come back to (keepView).
        let keepIndices = keepView ? animator.indices : []
        let keepPlane = keepView ? cubeSource?.planeIndex(animator.indices) : nil
        let keepImageSize = CGSize(width: imageWidth, height: imageHeight)

        DispatchQueue.global(qos: .userInitiated).async {
            do {
                // Memory-mapped when possible: cube planes are read only when shown.
                let data = try Data(contentsOf: url, options: .mappedIfSafe)
                // Annotations and regions saved by iFITS (APPLE_PENCIL_ANNOTATIONS and
                // DS9_REGIONS extensions), if any.
                let savedAnnotations = FITSAnnotationStore.readDrawingData(from: data)
                let savedRegions = FITSAnnotationStore.readRegionText(from: data)
                let result = try FITSDecoder.loadFITS(with: data, hdu: hdu)
                // Every HDU of the file (for the SNR page's noise menu), the shown HDU read straight
                // from the file (SNR reads every channel), and the SNR extension of an "_SNR_" file.
                let hdus = FITSHDUList.scan(data)
                let shownHDU = hdus.first { $0.index == result.hduIndex }
                let signal = shownHDU.flatMap { FITSImageReader(data: data, hdu: $0) }
                let snrMap = hdus.first {
                    $0.isImage && $0.index != result.hduIndex && $0.name.uppercased() == "SNR"
                        && $0.axes == result.axisLengths
                }.flatMap { FITSImageReader(data: data, hdu: $0) }
                // NAXIS ≥ 3 with more than one plane: a cube.
                let cube = CubeSource(data: data, width: result.width, height: result.height,
                                      bitpix: result.bitpix, dataOffset: result.dataOffset,
                                      bscale: result.bscale, bzero: result.bzero,
                                      axisLengths: result.axisLengths, header: result.headerDict)
                // Physical values = BZERO + BSCALE × stored value; BLANK (integer images) → NaN.
                // Flipped so FITS row 1 is at the bottom, like CARTA / DS9 (north up, RA along x).
                // When keeping the view of a cube, start on the same channel.
                let firstPlane = FITSPhysical.flipRows(
                    FITSPhysical.apply(result.floats, bscale: result.bscale, bzero: result.bzero,
                                       header: result.headerDict),
                    width: result.width, height: result.height)
                let keptPlane: (pixels: [Float], plane: Int)? = {
                    guard let keepPlane, keepPlane > 0, let cube, keepPlane < cube.planeCount,
                          let planePixels = cube.loadPlane(keepPlane) else { return nil }
                    return (planePixels, keepPlane)
                }()
                let pixels = keptPlane?.pixels ?? firstPlane
                let shownPlane = keptPlane?.plane ?? 0
                let stats = ImageStats(values: pixels, width: result.width)
                var settings = baseSettings
                (settings.clipMin, settings.clipMax) = stats.clipRange(percentile: percentile)
                let generatedImage = FITSRenderer.render(pixels, width: result.width,
                                                         height: result.height, settings: settings)

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

                    // Cube (or not): reset the animator to channel 0 (or the kept channel).
                    self.planeLoader.reset()
                    self.requestedPlane = shownPlane
                    self.cubeSource = cube
                    self.animator.configure(cube?.axes ?? [])
                    if shownPlane > 0, keepIndices.count == self.animator.indices.count {
                        for (axis, index) in keepIndices.enumerated() {
                            self.animator.setIndex(index, onAxis: axis)
                        }
                    }
                    // Nothing to undo in the new file, and nothing unsaved.
                    self.resetHistory()
                    // Spectra belong to the previous image too.
                    self.spectrum.imageChanged()
                    if cube == nil {
                        self.showMiniAnimator = false
                        if self.selectedMode == "C" { self.selectedMode = self.modeBeforeCube }
                    }
                    if self.selectedMode == "Z", cube == nil || signal == nil {
                        self.selectedMode = self.modeBeforeCube
                    }

                    // HDUs, the image for SNR, and the SNR map for Pixel Info.
                    self.loadToken = UUID()
                    self.loadedHDUs = hdus
                    self.loadedHDUIndex = result.hduIndex
                    self.signalImage = signal
                    self.snrImage = snrMap
                    self.snr.imageChanged(
                        candidates: hdus.filter {
                            $0.isImage && $0.index != result.hduIndex && $0.name.uppercased() != "SNR"
                        },
                        signalAxes: signal?.axes ?? [])

                    // Keep reading access to this file while it's open (cube planes are read
                    // from it later); give up access to the previous file.
                    if let previous = self.scopedURL, previous != url || scoped {
                        previous.stopAccessingSecurityScopedResource()
                    }
                    self.scopedURL = scoped ? url : nil
                    self.imageWidth = CGFloat(result.width)
                    self.imageHeight = CGFloat(result.height)

                    // Reset zoom / pan (kept when the new image is the same size and keepView is on).
                    let sameSize = keepImageSize == CGSize(width: result.width, height: result.height)
                    if !(keepView && sameSize) {
                        self.scale = 1.0
                        self.offset = .zero
                    }

                    self.isLoading = false
                    if generatedImage == nil {
                        self.errorMessage = "Failed to generate image from FITS data."
                    }
                }
            } catch {
                let message = error.localizedDescription
                DispatchQueue.main.async {
                    if scoped { url.stopAccessingSecurityScopedResource() }
                    if keepView {
                        // The current image stays; just say what went wrong.
                        self.saveError = "Couldn't open \(url.lastPathComponent): \(message)"
                    } else {
                        self.errorMessage = message
                    }
                    self.isLoading = false
                }
            }
        }
    }
}

/// Opening another file, waiting for the answer to "Save changes?".
struct PendingOpen: Identifiable {
    let id = UUID()
    let action: () -> Void
}
