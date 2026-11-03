//
//  ContentView+Toolbar.swift
//  iFITS Start
//
//  Top toolbar: file title, V A R S modes, cube, AR and Spectra buttons, header, grid.
//  In a narrow window (see WindowLayout) the buttons fold into one » menu, like Pages, which also
//  holds the V A R S modes once their buttons on the left are gone.
//

import SwiftUI

extension ContentView {
    @ToolbarContentBuilder
    var astroToolBar: some ToolbarContent {
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

    // MARK: Wide window

    @ToolbarContentBuilder
    var fullToolbarItems: some ToolbarContent {
        // Groups of three, like Pages: iOS 26 draws each group as its own glass capsule.
        // Cube mode, AR, and Spectra mode.
        ToolbarItemGroup(placement: .topBarTrailing) {
            Button { toggleCubeMode() } label: {
                CubeModeIcon()
                    .frame(width: 22, height: 22)
                    .foregroundStyle(Color.primary)
                    .padding(6)
                    // Orange circle while cube mode is on, like the V / A / R / S buttons.
                    .background {
                        if selectedMode == "C" {
                            Circle().fill(Color.orange.opacity(0.95))
                        }
                    }
            }
            .disabled(cubeSource == nil)
            .opacity(cubeSource == nil ? 0.4 : 1)
            .accessibilityLabel(selectedMode == "C" ? "Turn off cube mode" : "Cube mode")

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
                fileMenuItems
                Divider()
                settingsButton
            } label: {
                Image(systemName: "ellipsis")
            }
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
                    Label("Cube", systemImage: "square.stack.3d.up")
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
