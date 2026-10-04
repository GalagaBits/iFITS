//
//  AnnotationCanvas.swift
//  iFITS Start
//
//  PencilKit canvases for drawing and for showing annotations.
//

import SwiftUI
import UIKit
import PencilKit

/// The capture canvas: full-screen, fixed at 1×. Fingers / trackpad still move the image.
struct AnnotationCanvas: UIViewRepresentable {
    let model: AnnotationModel
    let isActive: Bool
    let fingerDrawing: Bool
    let screenToImage: CGAffineTransform
    var onTransform: (CGSize, CGFloat, CGPoint) -> Void
    /// A finger / pointer tap (never the Pencil).
    var onTap: ((CGPoint) -> Void)? = nil

    func makeCoordinator() -> ZoomPanController {
        // Fingers / trackpad move and zoom the image; the Pencil draws.
        ZoomPanController(ignorePencil: true)
    }

    func makeUIView(context: Context) -> CaptureCanvasView {
        let canvas = model.canvas
        context.coordinator.install(on: canvas)
        return canvas
    }

    func updateUIView(_ canvas: CaptureCanvasView, context: Context) {
        context.coordinator.onTransform = onTransform
        context.coordinator.onTap = onTap
        context.coordinator.dragEnabled = !fingerDrawing
        model.screenToImage = screenToImage
        model.setActive(isActive)
    }

    static func dismantleUIView(_ canvas: CaptureCanvasView, coordinator: ZoomPanController) {
        coordinator.uninstall()
    }
}

/// Shows the stored strokes with PencilKit's own rendering (real ink textures),
/// zoomed and scrolled to match the image exactly.
struct AnnotationDisplayCanvas: UIViewRepresentable {
    let model: AnnotationModel
    /// Changes when strokes are added/removed, so SwiftUI refreshes this view.
    let strokeCount: Int
    let imageWidth: CGFloat
    let imageHeight: CGFloat
    let scale: CGFloat
    let offset: CGSize
    let viewportSize: CGSize
    let maxScale: CGFloat

    func makeUIView(context: Context) -> PKCanvasView {
        model.displayCanvas
    }

    func updateUIView(_ canvas: PKCanvasView, context: Context) {
        model.syncDisplay(imageWidth: imageWidth, imageHeight: imageHeight, scale: scale,
                          offset: offset, viewport: viewportSize, maxScale: maxScale)
    }
}
