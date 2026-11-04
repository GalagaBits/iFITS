//
//  ContentView+Toolbar.swift
//  iFITS Start
//
//  Top toolbar: file title, V A R S modes, cube, AR and Spectra buttons, header, grid.
//  In a narrow window (see WindowLayout) the buttons fold into one » menu, like Pages, which also
//  holds the V A R S modes once their buttons on the left are gone.
//  In a vertical bar (iPhone Duo) the buttons are shown one by one, most important first, and the
//  system moves the ones that don't fit into the » menu.
//

import SwiftUI

extension ContentView {
    @ToolbarContentBuilder
    var astroToolBar: some ToolbarContent {
        if hasVerticalToolbar {
            // Only ever true on iOS 27.1 and later (VerticalToolbarReader).
            if #available(iOS 27.1, *) {
                verticalToolbarItems
            }
        } else {
            ToolbarItem(placement: .topBarLeading) {
                if windowLayout.foldsToolbar {
                    // A folder icon leaves more room for the file name.
                    Button { openFITSPicker() } label: { Image(systemName: "folder") }
                        .accessibilityLabel("Open File")
                } else {
                    Button("Open File") { openFITSPicker() }
                }
            }

            if windowLayout.foldsToolbar {
                ToolbarItem(placement: .topBarTrailing) {
                    foldedToolbarMenu
                }
            } else {
                fullToolbarItems
            }
        }
    }

    // MARK: Vertical bar (iPhone Duo)

    /// The buttons in separate glass groups, like on iPad. Top to bottom:
    /// - Open File
    /// - » : modes, FITS Header and Share (everything not shown as its own button)
    /// - Cube animator (tap: cube mode on, then play / pause; hold: cube mode off), AR, Spectra
    /// - Grid, Reset View
    /// - Undo, then ••• with the same actions as on iPad (Save, Save as Copy, Export and Send,
    ///   region files, Settings)
    /// When the column runs out of room, the system moves buttons (lowest priority first) into the
    /// ••• menu. Every button has an icon and a name: only icon buttons can go in a vertical bar.
    @available(iOS 27.1, *)
    @ToolbarContentBuilder
    var verticalToolbarItems: some ToolbarContent {
        ToolbarItem(placement: .topBarLeading) {
            Button { openFITSPicker() } label: {
                Label("Open File", systemImage: "folder")
            }
        }
        .visibilityPriority(.high)

        // Group 4: » with the modes and the commands that don't have their own button here.
        ToolbarItem(placement: .topBarTrailing) {
            verticalModesMenu
        }
        .visibilityPriority(.high)

        ToolbarSpacer(.fixed, placement: .topBarTrailing)

        // Group 3: cube animator, AR and Spectra mode.
        ToolbarItemGroup(placement: .topBarTrailing) {
            // The cube animator (play / pause icon). Outside cube mode: tap to turn cube mode on. In
            // cube mode (orange): tap to play or pause the channels. Hold for the Cube Mode switch.
            Menu {
                Toggle(isOn: Binding(get: { selectedMode == "C" }, set: { _ in toggleCubeMode() })) {
                    Label("Cube Mode", systemImage: cubeModeSymbol)
                }
            } label: {
                // Always the play / pause icon: play, or pause while the cube plays in cube mode.
                Label(selectedMode == "C" ? (animator.isPlaying ? "Pause" : "Play") : "Cube Mode",
                      systemImage: cubeModeSymbol)
                    .foregroundStyle(selectedMode == "C" ? Color.orange : Color.primary)
            } primaryAction: {
                if selectedMode == "C" {
                    animator.togglePlay()
                } else {
                    toggleCubeMode()
                }
            }
            .disabled(cubeSource == nil)

            Button { openAR() } label: {
                Label("3-D and AR View", systemImage: "arkit")
            }
            .disabled(!canOpenAR)

            Button { toggleSpectraMode() } label: {
                Label(selectedMode == "Z" ? "Turn Off Spectra Mode" : "Spectra Mode",
                      systemImage: "chart.xyaxis.line")
                    .foregroundStyle(selectedMode == "Z" ? Color.orange : Color.primary)
            }
            .disabled(!canUseSpectra)
        }

        ToolbarSpacer(.fixed, placement: .topBarTrailing)

        // Group 2: grid and reset view.
        ToolbarItemGroup(placement: .topBarTrailing) {
            Button { showGrid.toggle() } label: {
                Label(showGrid ? "Hide Grid" : "Show Grid", systemImage: "grid")
                    .foregroundStyle(showGrid ? Color.blue : Color.primary)
            }

            Button { resetView() } label: {
                Label("Reset View", systemImage: "arrow.counterclockwise")
            }
        }
        .visibilityPriority(.high)

        ToolbarSpacer(.fixed, placement: .topBarTrailing)

        // Group 1: undo, then ••• (always at the bottom).
        ToolbarItem(placement: .topBarTrailing) {
            undoButton
        }
        .visibilityPriority(.low)

        // ••• : the same actions as the iPad's ••• button. Buttons that don't fit above are added here
        // by the system.
        ToolbarOverflowMenu {
            fileMenuItems
            Divider()
            settingsButton
        }
    }

    /// The » menu in a vertical bar: the modes, the FITS Header and Share.
    var verticalModesMenu: some View {
        Menu {
            Section("Mode") {
                modeToggle("Visualization", systemImage: "v.circle", mode: "V")
                modeToggle("Annotation", systemImage: "a.circle", mode: "A")
                modeToggle("Regions", systemImage: "r.circle", mode: "R")
                modeToggle("Statistics", systemImage: "s.circle", mode: "S")
                Toggle(isOn: Binding(get: { selectedMode == "C" }, set: { _ in toggleCubeMode() })) {
                    Label("Cube", systemImage: cubeModeSymbol)
                }
                .disabled(cubeSource == nil)
                Toggle(isOn: Binding(get: { selectedMode == "Z" }, set: { _ in toggleSpectraMode() })) {
                    Label("Spectra", systemImage: "chart.xyaxis.line")
                }
                .disabled(!canUseSpectra)
            }

            Section {
                Button { showHeader() } label: {
                    Label("FITS Header", systemImage: "list.bullet.rectangle")
                }
                .disabled(headerCards.isEmpty)
                Button { shareFITS() } label: {
                    Label("Share…", systemImage: "square.and.arrow.up")
                }
                .disabled(loadedFileURL == nil)
            }
        } label: {
            Label("Modes and More", systemImage: "chevron.forward.2")
        }
        .menuOrder(.fixed)
    }

    // MARK: Wide window

    @ToolbarContentBuilder
    var fullToolbarItems: some ToolbarContent {
        // Groups of three, like Pages: iOS 26 draws each group as its own glass capsule.
        // Cube mode, AR, and Spectra mode.
        ToolbarItemGroup(placement: .topBarTrailing) {
            // The cube animator, as on iPhone Duo. Outside cube mode: tap to turn cube mode on. In cube
            // mode: tap to play or pause the channels. Hold for the Cube Mode switch.
            Menu {
                Toggle(isOn: Binding(get: { selectedMode == "C" }, set: { _ in toggleCubeMode() })) {
                    Label("Cube Mode", systemImage: cubeModeSymbol)
                }
            } label: {
                Image(systemName: cubeModeSymbol)
            } primaryAction: {
                if selectedMode == "C" {
                    animator.togglePlay()
                } else {
                    toggleCubeMode()
                }
            }
            .disabled(cubeSource == nil)
            .accessibilityLabel(selectedMode != "C" ? "Cube mode"
                                : (animator.isPlaying ? "Pause" : "Play"))

            // AR: the cube in 3-D, or placed in the room through the camera.
            Button { openAR() } label: { Image(systemName: "arkit") }
                .disabled(!canOpenAR)
                .accessibilityLabel("AR: view the cube in 3-D")

            // Spectra: the spectrum of a pixel or region along the cube's spectral axis.
            Button { toggleSpectraMode() } label: {
                Image(systemName: "chart.xyaxis.line")
                    .frame(width: 22, height: 22)
                    .foregroundStyle(Color.primary)
                    .padding(6)
                    // Orange circle while Spectra mode is on, like the cube button.
                    .background {
                        if selectedMode == "Z" {
                            Circle().fill(Color.orange.opacity(0.95))
                        }
                    }
            }
            .disabled(!canUseSpectra)
            .opacity(canUseSpectra ? 1 : 0.4)
            .accessibilityLabel(selectedMode == "Z" ? "Turn off Spectra mode" : "Spectra mode")
        }

        ToolbarSpacer(.fixed, placement: .topBarTrailing)

        ToolbarItemGroup(placement: .topBarTrailing) {
            Button { showHeader() } label: {
                Text("H").font(.title3.weight(.semibold))
            }
            .disabled(headerCards.isEmpty)
            .accessibilityLabel("FITS Header")

            Button { showGrid.toggle() } label: {
                Image(systemName: "grid")
                    .foregroundStyle(showGrid ? Color.blue : Color.primary)
            }
            .accessibilityLabel(showGrid ? "Hide grid" : "Show grid")

            Button { resetView() } label: {
                Image(systemName: "arrow.counterclockwise")
            }
            .accessibilityLabel("Reset view")
        }

        ToolbarSpacer(.fixed, placement: .topBarTrailing)

        ToolbarItemGroup(placement: .topBarTrailing) {
            // Undo the last edit (annotation or region); hold or right-click for Undo / Redo.
            undoButton

            // Share the FITS file; the share sheet also has "Export and Send…" (PNG / JPEG).
            Button { shareFITS() } label: { Image(systemName: "square.and.arrow.up") }
                .disabled(loadedFileURL == nil)
                .accessibilityLabel("Share")

            Menu {
                // The V A R S buttons are turned off in Settings: the modes are here instead.
                if !showModeButtons {
                    Section("Mode") {
                        modeToggle("Visualization", systemImage: "v.circle", mode: "V")
                        modeToggle("Annotation", systemImage: "a.circle", mode: "A")
                        modeToggle("Regions", systemImage: "r.circle", mode: "R")
                        modeToggle("Statistics", systemImage: "s.circle", mode: "S")
                    }
                }
                fileMenuItems
                Divider()
                settingsButton
            } label: {
                Image(systemName: "ellipsis")
            }
            .menuOrder(.fixed)
            .accessibilityLabel("More")
        }
    }

    // MARK: Narrow window

    /// Everything from the toolbar (and, in a small window, the V A R S buttons) in one menu, each
    /// item with its icon and name.
    var foldedToolbarMenu: some View {
        Menu {
            Section("Mode") {
                modeToggle("Visualization", systemImage: "v.circle", mode: "V")
                modeToggle("Annotation", systemImage: "a.circle", mode: "A")
                modeToggle("Regions", systemImage: "r.circle", mode: "R")
                modeToggle("Statistics", systemImage: "s.circle", mode: "S")
                Toggle(isOn: Binding(get: { selectedMode == "C" }, set: { _ in toggleCubeMode() })) {
                    Label("Cube", systemImage: cubeModeSymbol)
                }
                .disabled(cubeSource == nil)
                Toggle(isOn: Binding(get: { selectedMode == "Z" }, set: { _ in toggleSpectraMode() })) {
                    Label("Spectra", systemImage: "chart.xyaxis.line")
                }
                .disabled(!canUseSpectra)
            }

            Section {
                Button { openAR() } label: {
                    Label("3-D and AR View", systemImage: "arkit")
                }
                .disabled(!canOpenAR)
                Button { showHeader() } label: {
                    Label("FITS Header", systemImage: "list.bullet.rectangle")
                }
                .disabled(headerCards.isEmpty)
                Toggle(isOn: $showGrid) {
                    Label("Grid", systemImage: "grid")
                }
                Button { resetView() } label: {
                    Label("Reset View", systemImage: "arrow.counterclockwise")
                }
            }

            Section {
                Button { undoLastEdit() } label: {
                    Label(edits.undoTitle, systemImage: "arrow.uturn.backward")
                }
                .disabled(!edits.canUndo)
                Button { redoLastEdit() } label: {
                    Label(edits.redoTitle, systemImage: "arrow.uturn.forward")
                }
                .disabled(!edits.canRedo)
            }

            Section {
                Button { shareFITS() } label: {
                    Label("Share…", systemImage: "square.and.arrow.up")
                }
                .disabled(loadedFileURL == nil)
                fileMenuItems
            }

            settingsButton
        } label: {
            Image(systemName: "chevron.forward.2")
        }
        .menuOrder(.fixed)
        .accessibilityLabel("More")
    }

    /// A V A R S mode in the » menu (ticked while it's on).
    func modeToggle(_ title: String, systemImage: String, mode: String) -> some View {
        Toggle(isOn: Binding(get: { selectedMode == mode }, set: { _ in selectMode(mode) })) {
            Label(title, systemImage: systemImage)
        }
    }

    // MARK: Shared items

    /// Save, Save as Copy, Export and Send, and the region files.
    @ViewBuilder
    var fileMenuItems: some View {
        // Save = annotations and regions into the loaded FITS file.
        Group {
            Button { saveToOriginal() } label: {
                Label("Save", systemImage: "square.and.arrow.down")
            }
            Button { saveCopy() } label: {
                Label("Save as Copy…", systemImage: "doc.on.doc")
            }
            Button { exportImage() } label: {
                Label("Export and Send…", systemImage: "photo.badge.arrow.down")
            }
            Divider()
            Button { importRegions() } label: {
                Label("Import Regions (.reg)…", systemImage: "square.and.arrow.down.on.square")
            }
            Button { exportRegions() } label: {
                Label("Export Regions (.reg)…", systemImage: "square.and.arrow.up.on.square")
            }
            .disabled(regionStore.regions.isEmpty)
        }
        .disabled(loadedFileURL == nil)
    }

    var settingsButton: some View {
        Button { showSettings = true } label: {
            Label("Settings", systemImage: "gearshape")
        }
    }
}

/// Keeps `isVertical` up to date: true while the toolbar is a vertical bar (iPhone Duo). The
/// `toolbarVerticalEdge` value it reads is nil wherever the system isn't using a vertical bar, and
/// only exists on iOS 27.1 and later, so older systems always get false.
struct VerticalToolbarReader: ViewModifier {
    @Binding var isVertical: Bool

    func body(content: Content) -> some View {
        if #available(iOS 27.1, *) {
            content.modifier(VerticalToolbarEdgeReader(isVertical: $isVertical))
        } else {
            content
        }
    }
}

@available(iOS 27.1, *)
private struct VerticalToolbarEdgeReader: ViewModifier {
    @Binding var isVertical: Bool
    @Environment(\.toolbarVerticalEdge) private var verticalEdge

    func body(content: Content) -> some View {
        content.onChange(of: verticalEdge != nil, initial: true) { _, vertical in
            isVertical = vertical
        }
    }
}
