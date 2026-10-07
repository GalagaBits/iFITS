//
//  FITSMenuCommands.swift
//  iFITS Start
//
//  Menu bar: File, View, Mode, Visualization, Annotate, Cube, Regions.
//

import SwiftUI
import Combine

/// File, View, Mode, Visualization, Annotate, Cube and Regions menu items for the iPadOS menu bar
/// (also listed when you hold ⌘ with a hardware keyboard).
///
/// The items don't show live checkmarks on purpose: with SwiftUI menu commands, any change
/// to what the menu displays makes iPadOS rebuild the entire menu bar, which flickers on
/// every shortcut. Each item reads the current state when you use it instead.
struct FITSMenuCommands: Commands {
    @ObservedObject var center: FITSCommandCenter

    private var context: FITSCommandContext? { center.context }

    var body: some Commands {
        // Settings
        CommandGroup(replacing: .appSettings) {
            Button("Settings…") { context?.showSettings() }
                .keyboardShortcut(",", modifiers: .command)
        }
        // File
        CommandGroup(after: .newItem) {
            Button("Open FITS File…") { context?.openFile() }
                .keyboardShortcut("o", modifiers: .command)
            Button("Show Header in New Window") { context?.showHeader() }
                .keyboardShortcut("h", modifiers: [.command, .option])
            Divider()
            // Save = annotations and regions into the loaded FITS file.
            Button("Save") { context?.save() }
                .keyboardShortcut("s", modifiers: .command)
            Button("Save as Copy…") { context?.saveCopy() }
                .keyboardShortcut("s", modifiers: [.command, .shift])
            Divider()
            Button("Import Regions (.reg)…") { context?.importRegions() }
                .keyboardShortcut("o", modifiers: [.command, .shift])
            Button("Export Regions (.reg)…") { context?.exportRegions() }
                .keyboardShortcut("e", modifiers: [.command, .shift])
        }

        // View
        CommandGroup(after: .toolbar) {
            Section {
                Button("Show / Hide WCS Grid") { context?.showGrid.wrappedValue.toggle() }
                    .keyboardShortcut("g", modifiers: [.command, .shift])
            }
            Section {
                Menu("Move View") {
                    Button("Up") { context?.moveView(0, -1, false) }
                        .keyboardShortcut(.upArrow, modifiers: [])
                    Button("Down") { context?.moveView(0, 1, false) }
                        .keyboardShortcut(.downArrow, modifiers: [])
                    Button("Left") { context?.moveView(-1, 0, false) }
                        .keyboardShortcut(.leftArrow, modifiers: [])
                    Button("Right") { context?.moveView(1, 0, false) }
                        .keyboardShortcut(.rightArrow, modifiers: [])
                    Divider()
                    Button("Up More") { context?.moveView(0, -1, true) }
                        .keyboardShortcut(.upArrow, modifiers: .shift)
                    Button("Down More") { context?.moveView(0, 1, true) }
                        .keyboardShortcut(.downArrow, modifiers: .shift)
                    Button("Left More") { context?.moveView(-1, 0, true) }
                        .keyboardShortcut(.leftArrow, modifiers: .shift)
                    Button("Right More") { context?.moveView(1, 0, true) }
                        .keyboardShortcut(.rightArrow, modifiers: .shift)
                }
            }
            Section {
                Button("Zoom In") { context?.zoomIn() }
                    .keyboardShortcut("=", modifiers: .command)
                Button("Zoom Out") { context?.zoomOut() }
                    .keyboardShortcut("-", modifiers: .command)
                Button("Reset View") { context?.resetView() }
                    .keyboardShortcut("0", modifiers: .command)
            }
            Section {
                Button("Clear Pixel Info") { context?.clearPixelInfo() }
                    .keyboardShortcut("k", modifiers: [.command, .shift])
            }
        }

        // Mode (V / A / R / S)
        CommandMenu("Mode") {
            modeButton("V", "Visualization", key: "1")
            modeButton("A", "Annotation", key: "2")
            modeButton("R", "Regions", key: "3")
            modeButton("S", "Statistics", key: "4")
        }

        // Visualization (render configuration)
        CommandMenu("Visualization") {
            Button("Invert Colormap") { context?.renderSettings.wrappedValue.inverted.toggle() }
                .keyboardShortcut("i", modifiers: [.command, .option])

            Menu("Colormap") {
                ForEach(Colormap.allCases) { colormap in
                    Button(colormap.title) { context?.renderSettings.wrappedValue.colormap = colormap }
                }
            }

            Menu("Scaling") {
                ForEach(ScalingType.allCases) { scaling in
                    Button(scaling.title) { context?.renderSettings.wrappedValue.scaling = scaling }
                }
            }

            Menu("Clip Percentile") {
                ForEach(RenderConfigPanel.percentilePresets, id: \.self) { p in
                    Button(ClipSelection.percentText(p)) { context?.applyPercentile(p) }
                }
            }

            Divider()

            Button("Expand Render Configuration") { changePanel { $0.larger } }
                .keyboardShortcut(.upArrow, modifiers: [.command, .option])
            Button("Collapse Render Configuration") { changePanel { $0.smaller } }
                .keyboardShortcut(.downArrow, modifiers: [.command, .option])

            Menu("Render Configuration Size") {
                Button("Expanded") { changePanel { _ in .full } }
                Button("Compact") { changePanel { _ in .compact } }
                Button("Minimized") { changePanel { _ in .mini } }
            }
        }

        // Annotate
        CommandMenu("Annotate") {
            Button("Show / Hide Annotations") { context?.annotations.isVisible.toggle() }
                .keyboardShortcut("a", modifiers: [.command, .option])
            Button("Finger Drawing On / Off") { context?.annotations.fingerDrawing.toggle() }
            Button("Show / Hide Tool Palette") { context?.annotations.toolsVisible.toggle() }
            Divider()
            Button("Clear All Annotations") {
                if let model = context?.annotations, model.hasStrokes { model.clear() }
            }
        }

        // Cubes (NAXIS ≥ 3)
        CommandMenu("Cube") {
            Button("Cube Mode") { context?.cube(.toggleMode) }
                .keyboardShortcut("5", modifiers: .command)
            Button("Spectra Mode") { context?.cube(.toggleSpectra) }
                .keyboardShortcut("6", modifiers: .command)
            Divider()
            Button("Play / Pause") { context?.cube(.playPause) }
                .keyboardShortcut("p", modifiers: [.command, .option])
            Button("Next Channel") { context?.cube(.next) }
                .keyboardShortcut("]", modifiers: .command)
            Button("Previous Channel") { context?.cube(.previous) }
                .keyboardShortcut("[", modifiers: .command)
            Button("First Channel") { context?.cube(.first) }
                .keyboardShortcut("[", modifiers: [.command, .option])
            Button("Last Channel") { context?.cube(.last) }
                .keyboardShortcut("]", modifiers: [.command, .option])
            Divider()
            Button("Show Mini Animator") { context?.cube(.showMiniAnimator) }
        }

        // Regions & statistics
        CommandMenu("Regions") {
            Button("Select and Move") { context?.setRegionTool(nil) }
            ForEach(RegionShape.allCases) { shape in
                Button("Draw \(shape.title)") { context?.setRegionTool(shape) }
            }
            Divider()
            Button("Delete Selected Region") { context?.regions.deleteSelected() }
                .keyboardShortcut(.delete, modifiers: .command)
            Button("Deselect Region") { context?.regions.select(nil) }
            Divider()
            Button("Show Statistics") { context?.showStatistics() }
                .keyboardShortcut("s", modifiers: [.command, .option])
        }
    }

    // MARK: Helpers

    private func modeButton(_ code: String, _ title: String, key: Character) -> some View {
        Button(title) { context?.mode.wrappedValue = code }
            .keyboardShortcut(KeyEquivalent(key), modifiers: .command)
    }

    /// Resizes the Render Configuration box, switching to Visualization mode first if needed.
    private func changePanel(_ change: (PanelStage) -> PanelStage) {
        guard let context, context.hasImage() else { return }
        if context.mode.wrappedValue != "V" { context.mode.wrappedValue = "V" }
        let next = change(context.panelStage.wrappedValue)
        if next != context.panelStage.wrappedValue { context.panelStage.wrappedValue = next }
    }
}
