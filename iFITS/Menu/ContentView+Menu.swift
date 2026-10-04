//
//  ContentView+Menu.swift
//  iFITS Start
//
//  What the menu bar can see and do in this window.
//

import SwiftUI

extension ContentView {
    /// Everything the menu bar needs from this window.
    var commandContext: FITSCommandContext {
        FITSCommandContext(
            hasImage: { fitsImage != nil },
            mode: Binding(get: { selectedMode }, set: { mode in selectMode(mode) }),
            showGrid: $showGrid,
            renderSettings: $renderSettings,
            panelStage: Binding(get: { panelStage },
                                set: { stage in withAnimation(.bouncy(duration: 0.5, extraBounce: 0.12)) { panelStage = stage } }),
            annotations: annotations,
            regions: regionStore,
            openFile: { openFITSPicker() },
            showHeader: { showHeader() },
            moveView: { dx, dy, large in moveView(dx: dx, dy: dy, large: large) },
            zoomIn: { withAnimation(viewChangeAnimation) { applyTransform(scaleFactor: keyboardZoomStep) } },
            zoomOut: { withAnimation(viewChangeAnimation) { applyTransform(scaleFactor: 1 / keyboardZoomStep) } },
            resetView: { resetView() },
            applyPercentile: { applyPercentile($0) },
            clearPixelInfo: { withAnimation(.snappy) { inspectedPixel = nil } },
            save: { saveToOriginal() },
            saveCopy: { saveCopy() },
            importRegions: { importRegions() },
            exportRegions: { exportRegions() },
            showStatistics: {
                guard fitsImage != nil else { return }
                withAnimation(.snappy) { showStatsBox = true }
            },
            setRegionTool: { tool in
                guard fitsImage != nil else { return }
                selectMode("R")
                regionStore.tool = tool
            },
            cube: { command in cubeCommand(command) })
    }
}
