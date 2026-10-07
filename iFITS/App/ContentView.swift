//
//  ContentView.swift
//  iFITS Start
//
//  The main window: shared state and the view layout (body).
//  Feature code lives in ContentView+*.swift files in each feature's folder.
//

import SwiftUI
import UIKit
import UniformTypeIdentifiers
import Combine

struct ContentView: View {
    let modes = ["V", "A", "R", "S"]
    @State var selectedMode = "V"

    @State var showPicker = false
    @State var fitsImage: UIImage? = nil
    @State var isLoading = false
    @State var errorMessage: String? = nil
    @State var headerDict: [String: String] = [:]

    // Zoom / Pan State
    @State var scale: CGFloat = 1.0
    @State var offset: CGSize = .zero
    @State var viewportSize: CGSize = .zero

    let minScale: CGFloat = 0.2
    let keyboardPanStep: CGFloat = 60      // points per arrow-key press
    let keyboardZoomStep: CGFloat = 1.25   // zoom factor per ⌘+ / ⌘- press

    /// At full zoom-in, about this many image pixels span the screen's shorter side.
    /// Lower = you can zoom in further. (6 ≈ each pixel ~140 pt on an 11" iPad.)
    let pixelsAcrossAtMaxZoom: CGFloat = 6

    /// Maximum zoom, based on the image's size and the screen/window size:
    /// you can always zoom in until only a handful of image pixels fill the screen,
    /// whether the image is 50 or 10,000 pixels wide. Never less than 4×.
    var maxScale: CGFloat {
        guard viewportSize.width > 0, viewportSize.height > 0,
              imageWidth > 0, imageHeight > 0 else { return 50 }
        let baseScale = max(viewportSize.width / imageWidth, viewportSize.height / imageHeight) // pt per pixel at 1×
        let maxPointsPerPixel = min(viewportSize.width, viewportSize.height) / pixelsAcrossAtMaxZoom
        return max(4, maxPointsPerPixel / baseScale)
    }

    // Grid State
    @State var showGrid = false
    @State var imageWidth: CGFloat = 1.0
    @State var imageHeight: CGFloat = 1.0
    @State var wcs = WCS(header: [:])

    // Render Configuration (Visualization mode)
    @State var renderSettings = RenderSettings()
    @State var clipSelection: ClipSelection = .percentile(99.9)
    @State var imageStats: ImageStats? = nil
    @State var renderer = FITSRenderer()
    @State var panelStage: PanelStage = .full
    @State var panelHeight: CGFloat = 0

    /// Keyboard focus for the image area. iPadOS needs *something* in the window to hold focus,
    /// or the menu bar shows "No Menu Items" and no keyboard shortcut works.
    @FocusState var imageFocused: Bool

    // Pixel inspector (V / R / S modes): the pixel last hovered (trackpad / Pencil)
    // or double-tapped. Stays put when you switch input devices.
    @State var inspectedPixel: InspectedPixel? = nil
    /// Pixel Info shows the full table (true) or just the pixel and its value.
    @State var pixelInfoExpanded = true

    /// Double-tap only inspects once individual pixels are at least this big on screen (points).
    let minPixelSizeForTapInspect: CGFloat = 8

    // Annotations (A mode). Kept for the whole session, so they survive mode switches.
    @State var annotations = AnnotationModel()
    @Namespace var dockNamespace

    // Regions (R mode). Also kept for the whole session and drawn in every mode.
    @State var regionStore = RegionStore()
    /// The drag that's drawing or editing a region right now, if any.
    @State var regionDrag: RegionDrag? = nil
    @State var regionPanelExpanded = true

    // Statistics (S mode)
    /// The top-right statistics box. Closable; tapping S (even when already in S) shows it again.
    @State var showStatsBox = false
    @State var regionStats: RegionStatistics? = nil
    @State var statsPanelExpanded = true
    /// Page of the S-mode dock: 0 = statistics, 1 = SNR.
    @State var statsPage: Int? = 0
    /// SNR page settings (noise, cutoff) and progress.
    @State var snr = SNRModel()
    /// Changes every time a file is loaded, so statistics are recomputed.
    @State var imageGeneration = 0

    // Cubes (C mode): NAXIS ≥ 3
    /// The loaded cube (nil for a plain 2-D image).
    @State var cubeSource: CubeSource? = nil
    @State var animator = CubeAnimator()
    @State var planeLoader = CubePlaneLoader()
    /// The plane last asked for (so the same plane isn't decoded twice).
    @State var requestedPlane = 0
    @State var animatorExpanded = true
    /// The small bottom-right animator, shown in every mode once the animator is collapsed.
    @State var showMiniAnimator = false
    /// The V A R S mode to go back to when cube or Spectra mode is turned off.
    @State var modeBeforeCube = "V"
    /// The open file's security-scoped access stays on while it's loaded: cube planes are read
    /// from the (memory-mapped) file as they're shown.
    @State var scopedURL: URL? = nil

    // HDUs of the open file
    /// Every HDU of the open file, and which one is shown (0 = primary).
    @State var loadedHDUs: [FITSHDUInfo] = []
    @State var loadedHDUIndex = 0
    /// New each time an image is opened (an SNR result for an earlier image is then dropped).
    @State var loadToken = UUID()
    /// The shown image HDU, read straight from the file (SNR uses every channel).
    @State var signalImage: FITSImageReader? = nil
    /// The SNR extension of an "_SNR_" file made by iFITS (Pixel Info shows it).
    @State var snrImage: FITSImageReader? = nil
    /// The pop-up for choosing an image HDU (opening a file, or a noise file).
    @State var hduPicker: HDUPickerRequest? = nil
    /// The "_SNR_" file waiting to be saved.
    @State var snrExportDocument: FITSFileDocument? = nil
    @State var showSNRExporter = false
    @State var snrExportName = ""

    // AR / 3-D view of a cube, and the colorbar (bottom left)
    /// Set by the AR button; opens the AR view.
    @State var arSource: ARSource? = nil
    @State var colorbarExpanded = true
    /// Where the bottom dock and the V A R S buttons are (window coordinates), so the colorbar
    /// fits between them.
    @State var dockFrame: CGRect = .zero
    @State var modeButtonsBottom: CGFloat = 0

    // Spectra (Z mode): spectra of cubes along the spectral axis
    @State var spectrum = SpectrumModel()
    /// Connects this window to its Spectra window (pop-out button in the Spectra dock).
    @State var spectraLink = SpectraWindowLink()
    /// The Spectra "window" as a sheet, where extra windows aren't available.
    @State var showSpectraSheet = false

    struct PlaybackKey: Equatable {
        let playing: Bool
        let framesPerSecond: Int
    }

    var playbackKey: PlaybackKey {
        PlaybackKey(playing: animator.isPlaying && cubeSource != nil, framesPerSecond: animator.framesPerSecond)
    }

    // Region files (.reg)
    @State var importKind: ImportKind = .fits
    @State var regionExportDocument: RegionFileDocument? = nil
    @State var showRegionExporter = false

    /// What the file picker is opening.
    enum ImportKind {
        case fits, regions, noise

        var contentTypes: [UTType] {
            switch self {
            case .fits, .noise: [.fitsFile]
            case .regions: [RegionFileDocument.regionType, UTType(filenameExtension: "reg") ?? .plainText,
                            .plainText, .data]
            }
        }
    }

    /// What the statistics are computed for. A new value starts a new computation.
    struct StatsRequest: Equatable {
        let generation: Int
        let region: FITSRegion?
    }

    // FITS header window ("H" button)
    @State var headerCards: [FITSHeaderCard] = []
    @State var fileName = ""
    /// Fallback when the app can't open extra windows (shown as a sheet instead).
    @State var headerSheetDocument: FITSHeaderDocument? = nil

    // Saving annotations into the FITS file
    @State var loadedFileURL: URL? = nil
    @State var copyDocument: FITSFileDocument? = nil
    @State var showCopyExporter = false
    @State var saveMessage: String? = nil
    @State var saveError: String? = nil

    // Undo / redo (annotations and regions, one history for the window) and autosave
    @State var edits = EditHistory()
    @Environment(\.undoManager) var undoManager
    @Environment(\.scenePhase) var scenePhase
    /// Annotations and regions are saved into the FITS file as you work (Settings).
    @AppStorage("autosaveEnabled") var autosaveEnabled = true
    /// `edits.editCount` when the file was last saved (or opened).
    @State var savedEditCount = 0
    @State var isSaving = false
    /// A save asked for while another was running (runs when that one finishes).
    @State var queuedSave: (() -> Void)? = nil
    /// Autosave stopped for this file after a problem (reported once); Save tries again.
    @State var autosaveFailedURL: URL? = nil
    @State var showSettings = false

    // Sharing and exporting
    /// Where the FITS share sheet points from (under the toolbar's share button).
    @State var toolbarSharer = SharePresenter()
    /// The "Export and Send" sheet (PNG / JPEG of the image).
    @State var exportRequest: ExportRequest? = nil
    @Environment(\.openWindow) var openWindow
    /// Shared with the menu bar (FITSMenuCommands).
    @EnvironmentObject var commandCenter: FITSCommandCenter

    var showRenderPanel: Bool {
        selectedMode == "V" && fitsImage != nil && imageStats != nil
    }

    var isAnnotating: Bool {
        selectedMode == "A" && fitsImage != nil
    }

    /// Whether anything is docked at the bottom (for the WCS grid's label insets).
    var dockVisible: Bool {
        fitsImage != nil && (showRenderPanel || isAnnotating || selectedMode == "R" || selectedMode == "S"
                             || (selectedMode == "C" && cubeSource != nil)
                             || (selectedMode == "Z" && canUseSpectra))
    }

    /// The small bottom-right animator is on screen.
    var miniAnimatorVisible: Bool {
        showMiniAnimator && cubeSource != nil && fitsImage != nil && !(selectedMode == "C" && animatorExpanded)
    }

    /// The image has RA/Dec world coordinates.
    var hasCelestialWCS: Bool {
        wcs.isCelestial && headerDict["CTYPE1"] != nil
    }

    /// Pixel value units from the header (BUNIT).
    var valueUnit: String {
        headerDict["BUNIT"]?.trimmingCharacters(in: .whitespaces) ?? ""
    }

    var statsRequest: StatsRequest? {
        guard fitsImage != nil, showStatsBox || selectedMode == "S" else { return nil }
        return StatsRequest(generation: imageGeneration, region: regionStore.statsRegion)
    }

    /// Animate view changes only when there's nothing drawn, so annotations never lag the image.
    var viewChangeAnimation: Animation? {
        annotations.hasStrokes ? nil : .snappy
    }

    /// Current zoom limit, shared with the annotation layer.
    var currentMaxScale: CGFloat { maxScale }

    var body: some View {
        NavigationStack {
            // Undo / redo (the window's undo manager, region steps) and autosave.
            withUndoAndAutosave(windowLayers)
                .toolbar { astroToolBar }
                // The file name is the title. Tap it to rename the file (like Pages).
                .navigationTitle(Binding(get: { fileName.isEmpty ? "iFITS" : fileName },
                                         set: { renameLoadedFile(to: $0) }))
                .navigationBarTitleDisplayMode(.inline)
                .toolbarTitleMenu {
                    if loadedFileURL != nil {
                        RenameButton()
                        Divider()
                        Button { saveToOriginal() } label: {
                            Label("Save", systemImage: "square.and.arrow.down")
                        }
                        Button { saveCopy() } label: {
                            Label("Save as Copy…", systemImage: "doc.on.doc")
                        }
                        Button { showHeader() } label: {
                            Label("Show Header", systemImage: "list.bullet.rectangle")
                        }
                    } else {
                        Button { openFITSPicker() } label: {
                            Label("Open FITS File…", systemImage: "folder")
                        }
                    }
                }
                .alert("File Problem",
                       isPresented: Binding(get: { saveError != nil }, set: { if !$0 { saveError = nil } })) {
                    Button("OK", role: .cancel) {}
                } message: {
                    Text(saveError ?? "")
                }
                // White title (the file name) and bar items on the black image area, in light mode too.
                .toolbarBackground(Color.clear, for: .navigationBar)
                .toolbarBackground(.visible, for: .navigationBar)
                .toolbarColorScheme(.dark, for: .navigationBar)
                .sheet(item: $headerSheetDocument) { document in
                    HeaderWindowView(document: document, onClose: { headerSheetDocument = nil })
                }
                // SwiftUI's own file picker (FITS files, noise files, or .reg region files), which hands focus
                // back to the app when it closes.
                .fileImporter(isPresented: $showPicker, allowedContentTypes: importKind.contentTypes) { result in
                    if case .success(let url) = result {
                        switch importKind {
                        case .fits: prepareToOpen(url: url)
                        case .regions: loadRegionFile(url: url)
                        case .noise: loadNoiseFile(url: url)
                        }
                    }
                    // The file picker took keyboard focus; hand it back (see restoreImageFocus).
                    restoreImageFocus()
                }
        }
    }

    // MARK: - Layout pieces
    // The window is built from these smaller views so Swift can type-check each one quickly
    // (one huge `body` takes the compiler too long).

    /// Every layer of the window, plus what happens when state changes.
    var windowLayers: some View {
        ZStack {
            imageArea
            modeButtons
            topRightPanels
            saveToast
            bottomDock
            hduPickerHost
            snrExporterHost
            arHost
            spectraSheetHost
            settingsHost
            exportHost
            shareAnchorLayer
            colorbarLayer
            bottomRightLayer
        }
        .onChange(of: renderSettings) { _, newSettings in
            renderer.request(newSettings)
        }
        .onAppear {
            renderer.onImage = { image in
                if let image { fitsImage = image }
            }
            planeLoader.onPlane = { result in applyPlane(result) }
        }
        // Cubes: show the plane for the chosen channel(s).
        .onChange(of: animator.indices) { _, _ in
            showCurrentPlane()
        }
        // Collapsing the animator brings up the small bottom-right animator.
        .onChange(of: animatorExpanded) { _, expanded in
            if !expanded { withAnimation(.snappy) { showMiniAnimator = true } }
        }
        // Playback: one channel step per frame at the chosen frame rate.
        .task(id: playbackKey) {
            guard playbackKey.playing else { return }
            let interval = 1.0 / Double(max(1, playbackKey.framesPerSecond))
            while !Task.isCancelled {
                try? await Task.sleep(for: .seconds(interval))
                if Task.isCancelled { break }
                animator.tick()
            }
        }
        // Point the menu bar (FITSMenuCommands) at this window, once. Menu items read and
        // change the live state when used, so the menu bar never needs updating afterwards.
        // (Every update makes iPadOS rebuild the whole menu bar, which can flicker or break.)
        .onAppear {
            if commandCenter.context == nil { commandCenter.context = commandContext }
        }
        // Give the image area keyboard focus at launch, whenever focus could have been lost
        // (leaving A mode, closing the header sheet), and after editing Clip min / max.
        .onAppear { restoreImageFocus() }
        .onChange(of: selectedMode) { oldMode, mode in
            if mode != "A" { restoreImageFocus() }
            // Leaving Spectra mode from the menu bar (which sets the mode directly) does the same.
            if oldMode == "Z", mode != "Z", canUseSpectra {
                withAnimation(.snappy) { spectrum.showBox = true }
            }
        }
        .onChange(of: headerSheetDocument == nil) { _, closed in
            if closed { restoreImageFocus() }
        }
        .onReceive(NotificationCenter.default.publisher(for: .iFITSRestoreImageFocus)) { _ in
            restoreImageFocus()
        }
        // Statistics for the chosen region (or the whole image), recomputed off the main thread
        // whenever the region, its size or position, or the image changes.
        .task(id: statsRequest) {
            await updateStatistics()
        }
        // Spectra: recomputed off the main thread when the region, the Active pixel or the image
        // changes (not when the channel changes).
        .task(id: spectrumKeys) {
            await updateSpectrum()
        }
        // Deleted regions leave the spectra (and free their colour).
        .onChange(of: regionStore.statsCandidates.map(\.id)) { _, ids in
            spectrum.keepOnly(regions: Set(ids))
        }
        // The Spectra window (if open) follows this window's spectrum.
        .onAppear {
            spectraLink.model = spectrum
            SpectraWindowLink.register(spectraLink)
        }
        .onChange(of: spectraLinkKey) { _, key in
            if key != nil { pushSpectraLink() }
        }
    }

    /// Background layer: the image (or loading / error text), with the keyboard handlers.
    var imageArea: some View {
        ZStack {
            if isLoading {
                ProgressView("Loading FITS...")
            } else if let error = errorMessage {
                Text(error).foregroundColor(.red)
            } else if let uiImage = fitsImage {
                GeometryReader { geo in
                    imageCanvas(uiImage, size: geo.size)
                        .onChange(of: geo.size, initial: true) { _, newSize in
                            viewportSize = newSize
                        }
                }
            } else {
                Text("No FITS file loaded.")
                    .foregroundColor(.secondary)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color.black)
        .ignoresSafeArea()
        // The image area holds keyboard focus (no visible outline), so the window always
        // has a focused view: that keeps the menu bar filled and the shortcuts working.
        .focusable()
        .focusEffectDisabled()
        .focused($imageFocused)
        // Arrow keys move the image (Shift = bigger steps) whenever the image area has focus.
        .onKeyPress(keys: [.upArrow, .downArrow, .leftArrow, .rightArrow]) { press in
            let large = press.modifiers.contains(.shift)
            switch press.key {
            case .upArrow: moveView(dx: 0, dy: -1, large: large)
            case .downArrow: moveView(dx: 0, dy: 1, large: large)
            case .leftArrow: moveView(dx: -1, dy: 0, large: large)
            case .rightArrow: moveView(dx: 1, dy: 0, large: large)
            default: return .ignored
            }
            return .handled
        }
        // R mode: Esc puts the drawing tool away (or deselects); Delete removes the selected region.
        .onKeyPress(.escape) {
            guard selectedMode == "R" else { return .ignored }
            if regionStore.tool != nil {
                regionStore.tool = nil
            } else if regionStore.selectedID != nil {
                regionStore.select(nil)
            } else {
                return .ignored
            }
            return .handled
        }
        .onKeyPress(keys: [.delete, .deleteForward]) { _ in
            guard selectedMode == "R", regionStore.selectedID != nil else { return .ignored }
            regionStore.deleteSelected()
            return .handled
        }
        // Cubes: the space bar plays / pauses the animator.
        .onKeyPress(.space) {
            guard cubeSource != nil else { return .ignored }
            animator.togglePlay()
            return .handled
        }
        // "Save as Copy…": lets you choose where the copy goes.
        .fileExporter(isPresented: $showCopyExporter,
                      document: copyDocument,
                      contentType: FITSFileDocument.fitsType,
                      defaultFilename: (fileName as NSString).deletingPathExtension + "_copy") { result in
            switch result {
            case .success(let url): showSaveMessage("Saved copy \(url.lastPathComponent)")
            case .failure(let error): saveError = error.localizedDescription
            }
            copyDocument = nil
            restoreImageFocus()
        }
    }

    /// The image with its overlays: grid, gestures, inspected pixel, annotations, regions.
    func imageCanvas(_ uiImage: UIImage, size: CGSize) -> some View {
        Image(uiImage: uiImage)
            .resizable()
            .interpolation(.none)
            .aspectRatio(contentMode: .fill)
        // Pin the content to exactly the viewport size so its center
        // (the scaleEffect anchor) is the same as the viewport's center.
        .frame(width: size.width, height: size.height)
        .scaleEffect(scale)
        .offset(offset)
        .frame(width: size.width, height: size.height)
        .clipped()
        // The grid is drawn on top at screen resolution (not scaled
        // with the image), so lines stay sharp and text stays one size.
        .overlay {
            if showGrid {
                WCSGridView(wcs: wcs,
                            imageWidth: imageWidth,
                            imageHeight: imageHeight,
                            scale: scale,
                            offset: offset,
                            labelInsets: EdgeInsets(top: 80, leading: 130,
                                                    bottom: dockVisible ? panelHeight + 44 : 24,
                                                    trailing: 12))
                    .allowsHitTesting(false)
            }
        }
        // Gestures live on an untransformed overlay, so every location
        // is measured in plain viewport coordinates.
        .overlay {
            ZoomPanGestureView(
                onTransform: { translation, scaleFactor, anchor in
                    applyTransform(translation: translation, scaleFactor: scaleFactor, anchor: anchor)
                },
                onHover: { point, source in
                    inspect(atScreen: point, in: size, source: source)
                },
                onDoubleTap: { point in
                    // On a region: go to R mode with it selected.
                    if handleImageDoubleTap(atScreen: point) { return }
                    // Touch: only once individual pixels are visible.
                    guard pointsPerImagePixel(in: size) >= minPixelSizeForTapInspect else { return }
                    inspect(atScreen: point, in: size, source: .touch)
                },
                onInteraction: {
                    // Tapping or moving the image stops editing Clip min / max
                    // and gives keyboard focus back to the image (arrow keys).
                    endTextEditing()
                    restoreImageFocus()
                },
                // Tapping a region selects it (and switches to R mode).
                onTap: { point in handleImageTap(atScreen: point) },
                // In R mode, drags draw, move, stretch and rotate regions.
                regionDragBegan: { point in beginRegionDrag(atScreen: point) },
                regionDragChanged: { point in continueRegionDrag(atScreen: point) },
                regionDragEnded: { point in endRegionDrag(atScreen: point) },
                // Trackpad / mouse pointer shape over regions.
                pointerKind: { point in regionPointer(atScreen: point) })
        }
        // Outline of the inspected pixel (when it's big enough to see).
        .overlay {
            if let pixel = inspectedPixel, selectedMode != "A" {
                InspectedPixelOutline(
                    rect: CGRect(x: pixel.column, y: pixel.row, width: 1, height: 1)
                        .applying(imageToScreenTransform(in: size)))
            }
        }
        // Annotations: stored strokes drawn with the image's own transform
        // (always shown unless hidden), plus the PencilKit capture layer
        // on top (only touchable in A mode).
        .overlay {
            let toScreen = imageToScreenTransform(in: size)
            ZStack {
                AnnotationDisplayCanvas(model: annotations,
                                        strokeCount: annotations.strokes.count,
                                        imageWidth: imageWidth,
                                        imageHeight: imageHeight,
                                        scale: scale,
                                        offset: offset,
                                        viewportSize: size,
                                        maxScale: currentMaxScale)
                    .allowsHitTesting(false)
                    .opacity(annotations.isVisible ? 1 : 0)
                AnnotationCanvas(model: annotations,
                                 isActive: isAnnotating,
                                 fingerDrawing: annotations.fingerDrawing,
                                 screenToImage: toScreen.inverted(),
                                 onTransform: { translation, scaleFactor, anchor in
                                     applyTransform(translation: translation, scaleFactor: scaleFactor, anchor: anchor)
                                 },
                                 // A finger / pointer tap (not the Pencil) on a region selects it;
                                 // a double tap also switches to R mode.
                                 onTap: { point in handleImageTap(atScreen: point) },
                                 onDoubleTap: { point in handleImageDoubleTap(atScreen: point) })
                .allowsHitTesting(isAnnotating)
            }
        }
        // Regions, drawn on top of everything at screen resolution.
        .overlay {
            RegionOverlay(regions: regionStore.regions,
                          selectedID: regionStore.selectedID,
                          showHandles: selectedMode == "R",
                          imageToScreen: imageToScreenTransform(in: size),
                          imageHeight: imageHeight)
        }
    }

    /// The V A R S buttons on the left.
    var modeButtons: some View {
        HStack {
            VStack(spacing: 35) {
                VStack(spacing: 35) {
                    ForEach(modes, id: \.self) { mode in
                        Button(action: { selectMode(mode) }) {
                            ZStack {
                                if selectedMode == mode {
                                    Circle()
                                        .fill(.orange.opacity(0.95))
                                }

                                Circle()
                                    .glassEffect(.regular, in: Circle())

                                Text(mode)
                                    .font(.largeTitle)
                                    .foregroundColor(.primary)
                            }
                            .frame(width: 70, height: 70)
                            .contentShape(Circle())
                            .hoverEffect(.lift)
                        }
                        .buttonStyle(.plain)
                    }
                }
                // The colorbar goes below these buttons.
                .onGeometryChange(for: CGFloat.self) { $0.frame(in: .global).maxY } action: {
                    modeButtonsBottom = $0
                }
                Spacer()
            }
            .padding(.top, 60)
            .padding(.leading, 40)

            Spacer()
        }
        // "Export Regions (.reg)…" (kept off the views that already have a file picker or
        // exporter, since SwiftUI can mix them up when they share a view).
        .fileExporter(isPresented: $showRegionExporter,
                      document: regionExportDocument,
                      contentType: RegionFileDocument.regionType,
                      defaultFilename: (fileName as NSString).deletingPathExtension + "_regions") { result in
            switch result {
            case .success(let url): showSaveMessage("Exported regions to \(url.lastPathComponent)")
            case .failure(let error): saveError = error.localizedDescription
            }
            regionExportDocument = nil
            restoreImageFocus()
        }
    }

    /// Top right: Pixel Info, the statistics box and the spectrum box.
    @ViewBuilder
    var topRightPanels: some View {
        // Top right, under the toolbar buttons: the pixel inspector (not in A mode)
        // and the statistics box (every mode, until closed).
        if fitsImage != nil {
            VStack {
                HStack(alignment: .top) {
                    Spacer()
                    VStack(alignment: .trailing, spacing: 10) {
                        if let pixel = inspectedPixel, selectedMode != "A" {
                            PixelInfoPanel(pixel: pixel,
                                           value: renderer.value(column: pixel.column, row: pixel.row),
                                           wcs: wcs,
                                           header: headerDict,
                                           imageHeight: Int(imageHeight),
                                           channel: cubeSource == nil ? 0 : animator.index(onAxis: 0),
                                           snr: snrValue(at: pixel),
                                           expanded: $pixelInfoExpanded)
                                .transition(.opacity.combined(with: .scale(scale: 0.92, anchor: .topTrailing)))
                        }
                        // The statistics and spectrum boxes step aside while drawing (A mode)
                        // and come back afterwards.
                        if showStatsBox, selectedMode != "A" {
                            StatisticsBox(regionName: regionStore.statsRegion?.name ?? "Entire Image",
                                          stats: regionStats,
                                          unit: valueUnit,
                                          onClose: { withAnimation(.snappy) { showStatsBox = false } })
                                .transition(.opacity.combined(with: .scale(scale: 0.92, anchor: .topTrailing)))
                        }
                        spectrumBox
                    }
                }
                Spacer()
            }
            .padding(.top, 8)
            .padding(.trailing, 16)
        }
    }

    /// "Saved" confirmation, top center.
    @ViewBuilder
    var saveToast: some View {
        if let message = saveMessage {
            VStack {
                Text(message)
                    .font(.subheadline.weight(.semibold))
                    .padding(.horizontal, 16)
                    .padding(.vertical, 10)
                    .glassEffect(.regular, in: Capsule())
                Spacer()
            }
            .padding(.top, 8)
            .allowsHitTesting(false)
            .transition(.move(edge: .top).combined(with: .opacity))
        }
    }

    /// The bottom dock.
    var bottomDock: some View {
        // Bottom dock: Render Configuration (V), the Annotation bar (A), Regions (R) or
        // Statistics (S). They share one glass identity, so switching modes morphs one into the next.
        VStack {
            Spacer()
            GlassEffectContainer(spacing: 24) {
                dockPanel
                    // Where the panel really is (the colorbar moves up when it would overlap).
                    .onGeometryChange(for: CGRect.self) { $0.frame(in: .global) } action: { frame in
                        withAnimation(.smooth(duration: 0.3)) { dockFrame = frame }
                    }
                    .frame(maxWidth: .infinity, alignment: isAnnotating ? .leading : .center)
            }
            .onGeometryChange(for: CGFloat.self) { $0.size.height } action: { panelHeight = $0 }
            .padding(.horizontal, 24)
            .padding(.bottom, 8)
        }
    }

    /// The dock panel for the current mode.
    @ViewBuilder
    var dockPanel: some View {
        if showRenderPanel, let stats = imageStats {
            RenderConfigPanel(settings: $renderSettings,
                              stage: $panelStage,
                              clipSelection: clipSelection,
                              stats: stats,
                              glassNamespace: dockNamespace,
                              onSelectPercentile: { applyPercentile($0) },
                              onManualClip: { setManualClip($0, $1) })
        } else if isAnnotating {
            AnnotationBar(model: annotations, glassNamespace: dockNamespace)
        } else if selectedMode == "R", fitsImage != nil {
            RegionPanel(store: regionStore,
                        expanded: $regionPanelExpanded,
                        wcs: wcs,
                        hasWCS: hasCelestialWCS,
                        glassNamespace: dockNamespace,
                        onImport: { importRegions() },
                        onExport: { exportRegions() })
        } else if selectedMode == "S", fitsImage != nil {
            StatisticsDockPanel(store: regionStore,
                                expanded: $statsPanelExpanded,
                                page: $statsPage,
                                stats: regionStats,
                                unit: valueUnit,
                                glassNamespace: dockNamespace) {
                SNRControls(model: snr,
                            unit: valueUnit,
                            signalAxes: signalImage?.axes ?? [],
                            fileHDUs: noiseHDUCandidates,
                            productInfo: snrProductInfo,
                            onOpenNoiseFile: { openNoiseFilePicker() },
                            onCalculate: { calculateSNR() })
            }
        } else if selectedMode == "C", cubeSource != nil, fitsImage != nil {
            AnimatorPanel(animator: animator,
                          expanded: $animatorExpanded,
                          glassNamespace: dockNamespace)
        } else if selectedMode == "Z", canUseSpectra {
            spectraDock
        }
    }

    /// Hosts the HDU pop-up.
    var hduPickerHost: some View {
        // The HDU pop-up and the "_SNR_" file's save dialog each sit on their own empty,
        // zero-size view, since SwiftUI can mix up pickers, exporters and sheets that share a view.
        Color.clear
            .frame(width: 0, height: 0)
            .sheet(item: $hduPicker, onDismiss: { restoreImageFocus() }) { request in
                HDUPickerSheet(request: request,
                               onChoose: { hdu in choseHDU(hdu, for: request) },
                               onCancel: { hduPicker = nil })
            }
    }

    /// Hosts the save dialog for the "_SNR_" file.
    var snrExporterHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .fileExporter(isPresented: $showSNRExporter,
                          document: snrExportDocument,
                          contentType: FITSFileDocument.fitsType,
                          defaultFilename: snrExportName) { result in
                switch result {
                case .success(let url): openSavedSNRFile(url)
                case .failure(let error):
                    snrExportDocument = nil
                    saveError = error.localizedDescription
                }
                restoreImageFocus()
            }
    }

    /// Bottom right, above the dock: the small animator (every mode once the animator has been
    /// collapsed, until closed with its ✕).
    @ViewBuilder
    var bottomRightLayer: some View {
        if miniAnimatorVisible {
            VStack {
                Spacer()
                HStack {
                    Spacer()
                    MiniAnimatorBar(animator: animator) {
                        withAnimation(.snappy) {
                            showMiniAnimator = false
                            animator.isPlaying = false
                        }
                    }
                }
            }
            .padding(.trailing, 24)
            .padding(.bottom, dockVisible ? panelHeight + 20 : 16)
            .transition(.move(edge: .trailing).combined(with: .opacity))
        }
    }

    /// Bottom left, under the V A R S buttons: the vertical colorbar (whenever an image is open).
    /// It sits at the bottom, or just above the dock when the dock reaches across to the left, and
    /// gets shorter (or folds into its pill) when there isn't much room.
    @ViewBuilder
    var colorbarLayer: some View {
        // Steps aside while drawing (A mode), like the statistics and spectrum boxes.
        if fitsImage != nil, selectedMode != "A" {
            GeometryReader { geo in
                let frame = geo.frame(in: .global)
                let space = colorbarSpace(in: frame)
                if !space.hidden {
                    VStack(alignment: .leading, spacing: 0) {
                        Spacer(minLength: 0)
                        ImageColorbar(settings: $renderSettings,
                                      unit: valueUnit,
                                      barHeight: space.barHeight,
                                      pillOnly: space.pillOnly,
                                      expanded: $colorbarExpanded)
                    }
                    .padding(.leading, 40)
                    .padding(.bottom, max(0, frame.maxY - space.bottom))
                    .frame(width: frame.width, height: frame.height, alignment: .bottomLeading)
                }
            }
        }
    }

    /// Room for the colorbar: from below the V A R S buttons down to the bottom of the window, or to
    /// the top of the bottom dock if the dock reaches the colorbar's column.
    func colorbarSpace(in frame: CGRect) -> (bottom: CGFloat, barHeight: CGFloat, pillOnly: Bool, hidden: Bool) {
        var bottom = frame.maxY - 16
        let columnRight = frame.minX + 40 + VerticalColorbar.width + 12
        if dockVisible, dockFrame.width > 0, dockFrame.minX < columnRight {
            bottom = min(bottom, dockFrame.minY - 16)
        }
        let top = (modeButtonsBottom > 0 ? modeButtonsBottom : frame.minY + 445) + 24
        let room = bottom - top
        // The expanded colorbar adds about 50 points (chevron and padding) to its bar.
        let bar = min(300, room - 50)
        return (bottom, max(80, bar), bar < 100, room < 80)
    }

    // MARK: - Modes

    func selectMode(_ mode: String) {
        withAnimation(.bouncy(duration: 0.5, extraBounce: 0.1)) {
            // Leaving cube mode while a cube is playing keeps the small animator on screen.
            if selectedMode == "C", mode != "C", animator.isPlaying { showMiniAnimator = true }
            // Leaving Spectra mode brings up the spectrum box (top right).
            if selectedMode == "Z", mode != "Z", canUseSpectra { spectrum.showBox = true }
            selectedMode = mode
            // Tapping S (even when S is already on) brings the statistics box back.
            if mode == "S" { showStatsBox = true }
        }
    }
}
