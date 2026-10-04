//
//  FITSCommandCenter.swift
//  iFITS Start
//
//  Connects the menu bar to the open window.
//

import SwiftUI
import Combine

/// Cube menu commands.
enum CubeCommand {
    case toggleMode, playPause, next, previous, first, last, showMiniAnimator
}

/// What the frontmost FITS window shares with the menu bar. The bindings and closures read
/// and change the window's live state when a menu item is used.
struct FITSCommandContext {
    /// Live check (read when a menu item is used, so the menu bar never needs rebuilding).
    var hasImage: () -> Bool
    var mode: Binding<String>
    var showGrid: Binding<Bool>
    var renderSettings: Binding<RenderSettings>
    var panelStage: Binding<PanelStage>
    var annotations: AnnotationModel
    var regions: RegionStore

    var openFile: () -> Void
    var showHeader: () -> Void
    var moveView: (_ dx: CGFloat, _ dy: CGFloat, _ large: Bool) -> Void
    var zoomIn: () -> Void
    var zoomOut: () -> Void
    var resetView: () -> Void
    var applyPercentile: (Double) -> Void
    var clearPixelInfo: () -> Void
    var save: () -> Void
    var saveCopy: () -> Void
    var importRegions: () -> Void
    var exportRegions: () -> Void
    var showStatistics: () -> Void
    /// Switches to R mode with a drawing tool (nil = select / move).
    var setRegionTool: (RegionShape?) -> Void
    var cube: (CubeCommand) -> Void
}

/// Shared between the app's menu bar and the main window. The window registers itself
/// here, so menu items work no matter where keyboard focus is (e.g. while a sheet is open).
@MainActor
final class FITSCommandCenter: ObservableObject {
    @Published var context: FITSCommandContext?
}
