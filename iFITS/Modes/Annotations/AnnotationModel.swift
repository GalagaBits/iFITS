//
//  AnnotationModel.swift
//  iFITS Start
//
//  Annotations (A): PencilKit drawing state, undo and capture canvas.
//

import SwiftUI
import UIKit
import PencilKit

//
// How it works:
// • PencilKit is used only to CAPTURE strokes: a transparent canvas fixed at 1× in screen
//   space (Apple Pencil latency, pressure, the tool palette, the ruler).
// • When a stroke finishes, it's converted into image-pixel coordinates, stored in
//   AnnotationModel, and removed from the capture canvas.
// • Stored strokes are shown on a second, non-touchable PencilKit canvas (displayCanvas) that is
//   zoomed and scrolled with the same math as the image, so PencilKit renders them with their
//   real textures. Its drawing units are chosen so its zoom stays in a comfortable range across
//   the app's (image-size-based) zoom limits.
// • Capturing at 1× keeps pen widths sensible at every zoom (PencilKit clamps tool widths, so a
//   zoomed canvas would make strokes drawn while zoomed-in enormous).

/// The undo manager the capture canvas reports (so ⌘Z and the Edit menu work).
/// PencilKit records its own capture-canvas steps into it, but those are never used:
/// undo / redo / canUndo / canRedo all go to `history`, which holds only annotation steps.
/// No enable/disable counting, so nothing can get out of balance.
final class AnnotationUndoManager: UndoManager {
    /// The real annotation undo stack.
    let history = UndoManager()

    override init() {
        super.init()
        levelsOfUndo = 1          // keep PencilKit's ignored steps from piling up
    }

    override var canUndo: Bool { history.canUndo }
    override var canRedo: Bool { history.canRedo }
    override var undoActionName: String { history.undoActionName }
    override var redoActionName: String { history.redoActionName }
    override func undo() { history.undo() }
    override func redo() { history.redo() }
    override func removeAllActions() {
        super.removeAllActions()
        history.removeAllActions()
    }
}

/// Capture canvas whose undo goes to the annotation undo stack.
final class CaptureCanvasView: PKCanvasView {
    weak var annotationUndoManager: UndoManager?
    override var undoManager: UndoManager? { annotationUndoManager ?? super.undoManager }
}

@MainActor
@Observable
final class AnnotationModel: NSObject, PKCanvasViewDelegate, UIGestureRecognizerDelegate {
    let canvas = CaptureCanvasView()
    /// Shows the stored strokes (PencilKit rendering), zoomed to match the image.
    let displayCanvas = PKCanvasView()

    /// Finished strokes, in image-pixel coordinates (0…width, 0…height, top-left origin).
    private(set) var strokes: [PKStroke] = [] {
        didSet { applyDisplayDrawing() }
    }
    var canUndo = false
    var canRedo = false
    var hasStrokes: Bool { !strokes.isEmpty }
    /// Show or hide all annotations (in every mode).
    var isVisible = true
    /// Off: only Apple Pencil draws; fingers / trackpad move the image.
    /// On: fingers and pointer draw too; use two fingers to move and zoom.
    var fingerDrawing = false {
        didSet { applyInputPolicy() }
    }
    /// Show the PencilKit tool palette while in A mode.
    var toolsVisible = true {
        didSet { applyToolPicker() }
    }

    /// Screen → image-pixel transform for the current zoom/pan (set by AnnotationCanvas).
    @ObservationIgnored var screenToImage: CGAffineTransform = .identity {
        didSet {
            // The view moved: drop any just-finished stroke still shown on the capture layer,
            // so it can't drift away from the image.
            if screenToImage != oldValue { clearCaptureIfIdle() }
        }
    }

    /// Annotation undo (see AnnotationUndoManager): our steps live in `undo.history`.
    let undo = AnnotationUndoManager()
    @ObservationIgnored private var isActive = false
    @ObservationIgnored private var isClearingCapture = false
    @ObservationIgnored private var eraseSnapshot: [PKStroke]?
    @ObservationIgnored private var eraserGesture: UILongPressGestureRecognizer?
    @ObservationIgnored private var undoObservers: [NSObjectProtocol] = []

    // Capture → display hand-off
    @ObservationIgnored private var handedOffCount = 0
    @ObservationIgnored private var isUsingTool = false
    @ObservationIgnored private var clearWork: DispatchWorkItem?

    // Display canvas mapping
    @ObservationIgnored private var displayUnits: CGFloat = 0   // drawing units per image pixel

    /// The tool palette is created the first time A mode opens (keeps app launch fast).
    @ObservationIgnored private var toolPickerStorage: PKToolPicker?
    private var toolPicker: PKToolPicker {
        if let picker = toolPickerStorage { return picker }
        // No lasso: selections would act on the capture canvas, not the stored strokes.
        let pen = PKToolPickerInkingItem(type: .pen, color: .systemYellow, width: 4)
        let items: [PKToolPickerItem] = [
            pen,
            PKToolPickerInkingItem(type: .monoline, color: .systemGreen, width: 3),
            PKToolPickerInkingItem(type: .fountainPen, color: .systemRed, width: 4),
            PKToolPickerInkingItem(type: .marker, color: .systemYellow, width: 15),
            PKToolPickerInkingItem(type: .pencil, color: .white, width: 3),
            PKToolPickerInkingItem(type: .crayon, color: .systemPink, width: 10),
            PKToolPickerInkingItem(type: .watercolor, color: .systemBlue, width: 20),
            PKToolPickerEraserItem(type: .vector),
            PKToolPickerRulerItem()
        ]
        let picker = PKToolPicker(toolItems: items)
        picker.selectedToolItemIdentifier = pen.identifier
        picker.colorUserInterfaceStyle = .light
        picker.addObserver(canvas)
        toolPickerStorage = picker
        return picker
    }

    override init() {
        super.init()

        canvas.backgroundColor = .clear
        canvas.isOpaque = false
        // Keep ink colors exactly as picked (dark mode would swap black and white ink).
        canvas.overrideUserInterfaceStyle = .light
        canvas.tool = PKInkingTool(.pen, color: .systemYellow, width: 4)

        // Display canvas: never touchable; zoomed/scrolled by code to match the image.
        displayCanvas.backgroundColor = .clear
        displayCanvas.isOpaque = false
        displayCanvas.overrideUserInterfaceStyle = .light
        displayCanvas.isUserInteractionEnabled = false
        displayCanvas.drawingPolicy = .pencilOnly
        displayCanvas.contentInsetAdjustmentBehavior = .never
        displayCanvas.isScrollEnabled = false
        displayCanvas.allowsKeyboardScrolling = false
        displayCanvas.bounces = false
        displayCanvas.bouncesZoom = false
        displayCanvas.showsHorizontalScrollIndicator = false
        displayCanvas.showsVerticalScrollIndicator = false
        displayCanvas.minimumZoomScale = 0.0001
        displayCanvas.maximumZoomScale = 10_000
        displayCanvas.pinchGestureRecognizer?.isEnabled = false

        // PencilKit canvases are scroll views; iOS 26 would blur the area under the
        // toolbar for them (the "scroll edge effect"). Turn that off on every edge.
        for scrollView in [canvas as UIScrollView, displayCanvas as UIScrollView] {
            scrollView.topEdgeEffect.isHidden = true
            scrollView.bottomEdgeEffect.isHidden = true
            scrollView.leftEdgeEffect.isHidden = true
            scrollView.rightEdgeEffect.isHidden = true
        }

        // Fixed at 1× in screen space; it never scrolls or zooms.
        canvas.contentInsetAdjustmentBehavior = .never
        canvas.isScrollEnabled = false
        canvas.allowsKeyboardScrolling = false
        canvas.minimumZoomScale = 1
        canvas.maximumZoomScale = 1
        canvas.showsHorizontalScrollIndicator = false
        canvas.showsVerticalScrollIndicator = false
        canvas.delegate = self
        canvas.annotationUndoManager = undo

        // Vector eraser for stored strokes (PencilKit's eraser only sees the empty capture canvas).
        let eraser = UILongPressGestureRecognizer(target: self, action: #selector(handleErase(_:)))
        eraser.minimumPressDuration = 0
        eraser.allowableMovement = .greatestFiniteMagnitude
        eraser.delegate = self
        canvas.addGestureRecognizer(eraser)
        eraserGesture = eraser
        applyInputPolicy()

        for name in [Notification.Name.NSUndoManagerDidUndoChange,
                     .NSUndoManagerDidRedoChange,
                     .NSUndoManagerDidCloseUndoGroup] {
            undoObservers.append(NotificationCenter.default.addObserver(forName: name, object: undo.history, queue: .main) { [weak self] _ in
                MainActor.assumeIsolated { self?.refreshUndoState() }
            })
        }
    }

    // MARK: Capture → image space

    func canvasViewDidBeginUsingTool(_ canvasView: PKCanvasView) {
        isUsingTool = true
        clearWork?.cancel()
    }

    func canvasViewDidEndUsingTool(_ canvasView: PKCanvasView) {
        isUsingTool = false
        if handedOffCount > 0 { scheduleCaptureClear() }
    }

    func canvasViewDrawingDidChange(_ canvasView: PKCanvasView) {
        guard canvasView === canvas, !isClearingCapture else { return }
        let captured = canvasView.drawing.strokes
        if captured.count < handedOffCount { handedOffCount = captured.count; return }
        guard captured.count > handedOffCount else { return }

        let toImage = screenToImage
        let converted = captured[handedOffCount...].map { stroke -> PKStroke in
            var s = stroke
            s.transform = stroke.transform.concatenating(toImage)
            return s
        }
        handedOffCount = captured.count
        setStrokes(strokes + converted)
        // Leave the fresh stroke on the capture layer for a moment while the display
        // canvas renders it, so there's no flicker; then clear it.
        scheduleCaptureClear()
    }

    private func scheduleCaptureClear() {
        clearWork?.cancel()
        let work = DispatchWorkItem { [weak self] in
            MainActor.assumeIsolated { self?.clearCaptureIfIdle() }
        }
        clearWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: work)
    }

    private func clearCaptureIfIdle() {
        guard !isUsingTool, handedOffCount > 0 else { return }
        clearWork?.cancel()
        isClearingCapture = true
        canvas.drawing = PKDrawing()
        isClearingCapture = false
        handedOffCount = 0
    }

    // MARK: Display canvas (PencilKit rendering, zoomed to match the image)

    /// Called every time the image's zoom / pan / size changes.
    func syncDisplay(imageWidth iw: CGFloat, imageHeight ih: CGFloat, scale: CGFloat,
                     offset: CGSize, viewport: CGSize, maxScale: CGFloat) {
        let w = viewport.width, h = viewport.height
        guard w > 0, h > 0, iw > 0, ih > 0, scale > 0 else { return }

        let baseScale = max(w / iw, h / ih)                  // points per image pixel at 1×
        // Units picked so PencilKit's zoom is 1/√max at 1× and √max at full zoom-in:
        // centered in its comfortable range for the whole zoom range.
        let units = baseScale * max(maxScale, 1).squareRoot()
        if abs(units - displayUnits) > units * 1e-6 {
            displayUnits = units
            applyDisplayDrawing()
        }

        let zoom = scale * baseScale / units
        if abs(displayCanvas.zoomScale - zoom) > zoom * 1e-9 {
            displayCanvas.zoomScale = zoom
        }
        displayCanvas.contentInset = .zero
        displayCanvas.contentSize = CGSize(width: iw * units * zoom, height: ih * units * zoom)
        // Put the image's top-left corner where the rendered image's top-left corner is.
        displayCanvas.contentOffset = CGPoint(x: scale * iw * baseScale / 2 - w / 2 - offset.width,
                                              y: scale * ih * baseScale / 2 - h / 2 - offset.height)
    }

    /// Rebuilds the display drawing from the stored strokes (image pixels → display units).
    private func applyDisplayDrawing() {
        guard displayUnits > 0 else { return }
        let toDisplay = CGAffineTransform(scaleX: displayUnits, y: displayUnits)
        displayCanvas.drawing = PKDrawing(strokes: strokes.map { stroke in
            var s = stroke
            s.transform = stroke.transform.concatenating(toDisplay)
            return s
        })
    }

    // MARK: Editing (all undoable)

    private func setStrokes(_ newStrokes: [PKStroke]) {
        let old = strokes
        strokes = newStrokes
        undo.history.registerUndo(withTarget: self) { model in
            MainActor.assumeIsolated { model.setStrokes(old) }
        }
        refreshUndoState()
    }

    private func refreshUndoState() {
        canUndo = undo.canUndo
        canRedo = undo.canRedo
    }

    func undoLast() {
        clearCaptureIfIdle()
        if undo.canUndo { undo.undo() }
        refreshUndoState()
    }

    func redoLast() {
        clearCaptureIfIdle()
        if undo.canRedo { undo.redo() }
        refreshUndoState()
    }

    /// Clears everything (undoable with Undo / ⌘Z).
    func clear() {
        clearCaptureIfIdle()
        setStrokes([])
    }

    /// The annotations as PencilKit data (image-pixel coordinates), or nil if there are none.
    func drawingData() -> Data? {
        strokes.isEmpty ? nil : PKDrawing(strokes: strokes).dataRepresentation()
    }

    /// Replaces the annotations with ones loaded from a file (not undoable).
    func restore(_ restored: [PKStroke]) {
        strokes = restored
        undo.removeAllActions()
        refreshUndoState()
    }

    /// Wipes annotations and their undo history (used when a new file is opened).
    func removeAll() {
        clearWork?.cancel()
        strokes = []
        isClearingCapture = true
        canvas.drawing = PKDrawing()
        isClearingCapture = false
        handedOffCount = 0
        undo.removeAllActions()
        refreshUndoState()
    }

    // MARK: Eraser

    func gestureRecognizerShouldBegin(_ gestureRecognizer: UIGestureRecognizer) -> Bool {
        canvas.tool is PKEraserTool
    }

    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer,
                           shouldRecognizeSimultaneouslyWith other: UIGestureRecognizer) -> Bool {
        true
    }

    @objc private func handleErase(_ g: UILongPressGestureRecognizer) {
        switch g.state {
        case .began:
            clearCaptureIfIdle()
            eraseSnapshot = strokes
            erase(atScreen: g.location(in: canvas))
        case .changed:
            erase(atScreen: g.location(in: canvas))
        case .ended, .cancelled, .failed:
            if let before = eraseSnapshot, before.count != strokes.count {
                let after = strokes
                strokes = before
                setStrokes(after)       // one undo step per eraser swipe
            }
            eraseSnapshot = nil
        default:
            break
        }
    }

    private func erase(atScreen point: CGPoint) {
        let width = (canvas.tool as? PKEraserTool)?.width ?? 20
        let p = point.applying(screenToImage)
        let r = width / 2 * AnnotationModel.scaleOf(screenToImage)
        let kept = strokes.filter { !AnnotationModel.stroke($0, hits: p, radius: r) }
        if kept.count != strokes.count { strokes = kept }
    }

    private static func stroke(_ s: PKStroke, hits p: CGPoint, radius r: CGFloat) -> Bool {
        guard s.renderBounds.insetBy(dx: -r, dy: -r).contains(p) else { return false }
        let t = s.transform
        let sc = AnnotationModel.scaleOf(t)
        var previous: CGPoint?
        for pt in s.path.interpolatedPoints(by: .parametricStep(0.25)) {
            let q = pt.location.applying(t)
            let reach = r + pt.size.width * sc / 2
            let d = previous.map { distance(from: p, toSegment: $0, q) } ?? hypot(p.x - q.x, p.y - q.y)
            if d <= reach { return true }
            previous = q
        }
        return false
    }

    static func scaleOf(_ t: CGAffineTransform) -> CGFloat {
        sqrt(abs(t.a * t.d - t.b * t.c))
    }

    private static func distance(from p: CGPoint, toSegment a: CGPoint, _ b: CGPoint) -> CGFloat {
        let dx = b.x - a.x, dy = b.y - a.y
        let len2 = dx * dx + dy * dy
        guard len2 > 0 else { return hypot(p.x - a.x, p.y - a.y) }
        let t = max(0, min(1, ((p.x - a.x) * dx + (p.y - a.y) * dy) / len2))
        return hypot(p.x - (a.x + t * dx), p.y - (a.y + t * dy))
    }

    // MARK: Mode / input

    private func applyInputPolicy() {
        canvas.drawingPolicy = fingerDrawing ? .anyInput : .pencilOnly
        var types = [NSNumber(value: UITouch.TouchType.pencil.rawValue)]
        if fingerDrawing {
            types += [NSNumber(value: UITouch.TouchType.direct.rawValue),
                      NSNumber(value: UITouch.TouchType.indirectPointer.rawValue)]
        }
        eraserGesture?.allowedTouchTypes = types
    }

    /// Called when entering or leaving A mode.
    func setActive(_ active: Bool) {
        guard active != isActive else { return }
        isActive = active
        // Defer so the canvas is in the window before it becomes first responder.
        Task { @MainActor in self.applyToolPicker() }
    }

    private func applyToolPicker() {
        // Don't build the palette just to hide it.
        if isActive || toolPickerStorage != nil {
            toolPicker.setVisible(isActive && toolsVisible, forFirstResponder: canvas)
        }
        if isActive {
            canvas.becomeFirstResponder()
        } else {
            canvas.resignFirstResponder()
        }
    }
}
