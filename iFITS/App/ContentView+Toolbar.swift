//
//  ContentView+Toolbar.swift
//  iFITS Start
//
//  Top toolbar: file title, V A R S modes, cube, AR and Spectra buttons, header, grid.
//

import SwiftUI

extension ContentView {
    @ToolbarContentBuilder
    var astroToolBar: some ToolbarContent {
        ToolbarItem(placement: .topBarLeading) {
            Button("Open File") { openFITSPicker() }
        }

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
            Button { annotations.undoLast() } label: {
                Image(systemName: "arrow.uturn.backward")
            }
            .disabled(!annotations.canUndo)
            .accessibilityLabel("Undo")

            Button {} label: { Image(systemName: "square.and.arrow.up") }
                .accessibilityLabel("Share")

            Menu {
                // Save = annotations and regions into the loaded FITS file.
                Button { saveToOriginal() } label: {
                    Label("Save", systemImage: "square.and.arrow.down")
                }
                Button { saveCopy() } label: {
                    Label("Save as Copy…", systemImage: "doc.on.doc")
                }
                Divider()
                Button { importRegions() } label: {
                    Label("Import Regions (.reg)…", systemImage: "square.and.arrow.down.on.square")
                }
                Button { exportRegions() } label: {
                    Label("Export Regions (.reg)…", systemImage: "square.and.arrow.up.on.square")
                }
                .disabled(regionStore.regions.isEmpty)
            } label: {
                Image(systemName: "ellipsis")
            }
            .disabled(loadedFileURL == nil)
            .accessibilityLabel("More")
        }
    }
}
