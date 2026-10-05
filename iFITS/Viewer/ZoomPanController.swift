//
//  ZoomPanController.swift
//  iFITS Start
//
//  UIKit gestures for the image: pan, pinch, scroll, taps, hover and pointer shapes.
//

import SwiftUI
import UIKit

/// Transparent UIKit layer that handles:
/// - Pinch (fingers or trackpad) → zoom around the fingers / pointer, and follow the fingers
/// - One-finger drag or click-drag → pan
/// - Two-finger trackpad scroll or mouse wheel → pan
/// - ⌘ + scroll → zoom around the pointer (handy with a mouse wheel)
/// - Taps, and (in R mode) drags that draw or edit regions, plus the pointer's region shapes
struct ZoomPanGestureView: UIViewRepresentable {
    /// (translation, scaleFactor, anchor measured from the view's center)
    var onTransform: (CGSize, CGFloat, CGPoint) -> Void
    /// Trackpad / mouse pointer or Apple Pencil hovering over the image.
    var onHover: ((CGPoint, InspectSource) -> Void)? = nil
    /// Double tap with a finger (or Pencil).
    var onDoubleTap: ((CGPoint) -> Void)? = nil
    /// Any tap, or the start of a drag / pinch, on the image.
    var onInteraction: (() -> Void)? = nil
    /// A single tap, with its location.
    var onTap: ((CGPoint) -> Void)? = nil
    /// Start of a one-finger / click drag: return true to use it for regions instead of panning.
    var regionDragBegan: ((CGPoint) -> Bool)? = nil
    var regionDragChanged: ((CGPoint) -> Void)? = nil
    /// End of a region drag (nil location = cancelled).
    var regionDragEnded: ((CGPoint?) -> Void)? = nil
    /// Pointer shape at a location (nil = the normal pointer).
    var pointerKind: ((CGPoint) -> RegionPointer?)? = nil

    func makeCoordinator() -> ZoomPanController { ZoomPanController(inspection: true) }

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        view.backgroundColor = .clear
        context.coordinator.install(on: view)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        context.coordinator.onTransform = onTransform
        context.coordinator.onHover = onHover
        context.coordinator.onDoubleTap = onDoubleTap
        context.coordinator.onInteraction = onInteraction
        context.coordinator.onTap = onTap
        context.coordinator.regionDragBegan = regionDragBegan
        context.coordinator.regionDragChanged = regionDragChanged
        context.coordinator.regionDragEnded = regionDragEnded
        context.coordinator.pointerKind = pointerKind
    }
}

/// The pan / pinch / scroll recognizers, reusable on any view (the plain gesture layer,
/// and the PencilKit canvas in A mode, where the Pencil draws and fingers move the image).
final class ZoomPanController: NSObject, UIGestureRecognizerDelegate, UIPointerInteractionDelegate {
    var onTransform: (CGSize, CGFloat, CGPoint) -> Void = { _, _, _ in }
    var onHover: ((CGPoint, InspectSource) -> Void)?
    var onDoubleTap: ((CGPoint) -> Void)?
    var onInteraction: (() -> Void)?
    var onTap: ((CGPoint) -> Void)?
    var regionDragBegan: ((CGPoint) -> Bool)?
    var regionDragChanged: ((CGPoint) -> Void)?
    var regionDragEnded: ((CGPoint?) -> Void)?
    var pointerKind: ((CGPoint) -> RegionPointer?)?

    /// When false, one-finger / pointer drags and taps are left alone (used while finger drawing is on).
    var dragEnabled = true {
        didSet {
            drag?.isEnabled = dragEnabled
            tap?.isEnabled = dragEnabled
            doubleTap?.isEnabled = dragEnabled
        }
    }

    // Lower = slower trackpad zoom. 1.0 = raw system speed. Try 0.3–0.6.
    var trackpadPinchSensitivity: CGFloat = 1.0
    // Finger pinch speed on the screen (1.0 = image stays glued to your fingers).
    var touchPinchSensitivity: CGFloat = 1.0

    private let ignorePencil: Bool
    private let inspection: Bool
    /// Double taps are reported (always on with `inspection`).
    private let doubleTaps: Bool
    private weak var doubleTap: UITapGestureRecognizer?
    private var installed: [UIGestureRecognizer] = []
    private weak var drag: UIPanGestureRecognizer?
    private weak var tap: UITapGestureRecognizer?
    private var pointerShapes: UIPointerInteraction?
    /// The current one-finger / click drag is editing a region (not panning).
    private var regionDragging = false

    private var lastPinchLocation: CGPoint?
    private var lastPinchTouchCount = 0
    private var lastPinchScale: CGFloat = 1
    private var pinchIsFromTrackpad = false

    init(ignorePencil: Bool = false, inspection: Bool = false, doubleTaps: Bool = false) {
        self.ignorePencil = ignorePencil
        self.inspection = inspection
        self.doubleTaps = doubleTaps || inspection
        super.init()
    }

    func install(on view: UIView) {
        uninstall()

        // One-finger drag (touch) or click-drag (trackpad/mouse)
        let drag = UIPanGestureRecognizer(target: self, action: #selector(handleDrag(_:)))
        drag.maximumNumberOfTouches = 1
        if ignorePencil {
            // The Pencil is for drawing, never for moving the image.
            drag.allowedTouchTypes = [NSNumber(value: UITouch.TouchType.direct.rawValue),
                                      NSNumber(value: UITouch.TouchType.indirectPointer.rawValue)]
        }
        drag.isEnabled = dragEnabled

        // Two-finger trackpad scroll / mouse wheel only (no touches)
        let scroll = UIPanGestureRecognizer(target: self, action: #selector(handleScroll(_:)))
        scroll.allowedScrollTypesMask = .all
        scroll.allowedTouchTypes = []

        // Pinch with fingers or on the trackpad
        let pinch = UIPinchGestureRecognizer(target: self, action: #selector(handlePinch(_:)))

        // Single tap (doesn't wait for a double tap): selects regions, ends text editing.
        // Never the Pencil on the drawing canvas, where the Pencil draws.
        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap(_:)))
        if ignorePencil {
            tap.allowedTouchTypes = [NSNumber(value: UITouch.TouchType.direct.rawValue),
                                     NSNumber(value: UITouch.TouchType.indirectPointer.rawValue)]
        }
        tap.isEnabled = dragEnabled

        var recognizers: [UIGestureRecognizer] = [drag, scroll, pinch, tap]
        if inspection {
            // Pointer and Apple Pencil hover (iPadOS reports both through this recognizer).
            recognizers.append(UIHoverGestureRecognizer(target: self, action: #selector(handleHover(_:))))
            // The pointer changes shape over regions (see RegionPointer).
            let pointer = UIPointerInteraction(delegate: self)
            view.addInteraction(pointer)
            pointerShapes = pointer
        }
        if doubleTaps {
            let doubleTap = UITapGestureRecognizer(target: self, action: #selector(handleDoubleTap(_:)))
            doubleTap.numberOfTapsRequired = 2
            if ignorePencil {
                doubleTap.allowedTouchTypes = [NSNumber(value: UITouch.TouchType.direct.rawValue),
                                               NSNumber(value: UITouch.TouchType.indirectPointer.rawValue)]
            }
            doubleTap.isEnabled = dragEnabled
            recognizers.append(doubleTap)
            self.doubleTap = doubleTap
        }
        for g in recognizers {
            g.delegate = self
            view.addGestureRecognizer(g)
        }
        installed = recognizers
        self.drag = drag
        self.tap = tap
    }

    func uninstall() {
        for g in installed { g.view?.removeGestureRecognizer(g) }
        installed = []
        if let pointerShapes { pointerShapes.view?.removeInteraction(pointerShapes) }
        pointerShapes = nil
    }

    // MARK: Pointer shapes

    func pointerInteraction(_ interaction: UIPointerInteraction, regionFor request: UIPointerRegionRequest,
                            defaultRegion: UIPointerRegion) -> UIPointerRegion? {
        guard let kind = pointerKind?(request.location) else { return nil }
        // A tiny region around the pointer, so iPadOS asks again as soon as it moves.
        let p = request.location
        return UIPointerRegion(rect: CGRect(x: p.x - 0.5, y: p.y - 0.5, width: 1, height: 1),
                               identifier: kind.identifier)
    }

    func pointerInteraction(_ interaction: UIPointerInteraction, styleFor region: UIPointerRegion) -> UIPointerStyle? {
        guard let id = region.identifier as? String, let kind = RegionPointer(identifier: id) else { return nil }
        return kind.style
    }

    private func anchor(_ point: CGPoint, in view: UIView) -> CGPoint {
        CGPoint(x: point.x - view.bounds.midX, y: point.y - view.bounds.midY)
    }

    @objc func handleHover(_ g: UIHoverGestureRecognizer) {
        guard let view = g.view else { return }
        switch g.state {
        case .began, .changed:
            // Apple Pencil hover reports a height above the screen; the pointer doesn't.
            onHover?(g.location(in: view), g.zOffset > 0 ? .pencil : .pointer)
        default:
            break   // keep showing the last pixel when the pointer / Pencil leaves
        }
    }

    @objc func handleTap(_ g: UITapGestureRecognizer) {
        guard g.state == .ended, let view = g.view else { return }
        onInteraction?()
        onTap?(g.location(in: view))
    }

    @objc func handleDoubleTap(_ g: UITapGestureRecognizer) {
        guard let view = g.view, g.state == .ended else { return }
        onDoubleTap?(g.location(in: view))
    }

    @objc func handleDrag(_ g: UIPanGestureRecognizer) {
        guard let view = g.view else { return }
        let location = g.location(in: view)
        switch g.state {
        case .began:
            onInteraction?()
            // Where the finger / pointer went down (the drag starts after a little movement).
            let t = g.translation(in: view)
            let start = CGPoint(x: location.x - t.x, y: location.y - t.y)
            if regionDragBegan?(start) == true {
                regionDragging = true
                regionDragChanged?(location)
                return
            }
        case .changed:
            if regionDragging {
                regionDragChanged?(location)
                return
            }
        case .ended, .cancelled, .failed:
            if regionDragging {
                regionDragging = false
                regionDragEnded?(g.state == .ended ? location : nil)
                pointerShapes?.invalidate()
                return
            }
        default:
            break
        }
        let t = g.translation(in: view)
        g.setTranslation(.zero, in: view)
        onTransform(CGSize(width: t.x, height: t.y), 1, .zero)
    }

    @objc func handleScroll(_ g: UIPanGestureRecognizer) {
        guard let view = g.view else { return }
        let t = g.translation(in: view)
        g.setTranslation(.zero, in: view)

        if g.modifierFlags.contains(.command) {
            let factor = exp(t.y / 400)   // bigger number = slower ⌘-scroll zoom
            onTransform(.zero, factor, anchor(g.location(in: view), in: view))
        } else {
            onTransform(CGSize(width: t.x, height: t.y), 1, .zero)
        }
    }

    // Trackpad pinches arrive as "transform" events instead of touches.
    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldReceive event: UIEvent) -> Bool {
        if gestureRecognizer is UIPinchGestureRecognizer {
            pinchIsFromTrackpad = (event.type == .transform)
        }
        return true
    }

    @objc func handlePinch(_ g: UIPinchGestureRecognizer) {
        guard let view = g.view else { return }
        let location = g.location(in: view)

        switch g.state {
        case .began:
            onInteraction?()
            lastPinchLocation = location
            lastPinchTouchCount = g.numberOfTouches
            lastPinchScale = g.scale
            // Cancel any one-finger drag so it doesn't fight the pinch.
            drag?.isEnabled = false
            drag?.isEnabled = dragEnabled

        case .changed:
            // Use the change since the last update instead of resetting g.scale,
            // which trackpad pinches don't reliably honor (that made zoom compound).
            guard lastPinchScale > 0 else { lastPinchScale = g.scale; return }
            let rawFactor = g.scale / lastPinchScale
            lastPinchScale = g.scale

            let isTrackpad = pinchIsFromTrackpad || g.numberOfTouches < 2
            let sensitivity = isTrackpad ? trackpadPinchSensitivity : touchPinchSensitivity
            let factor = pow(rawFactor, sensitivity)

            var translation = CGSize.zero
            // Follow the fingers, but skip the jump when a finger is added/lifted.
            if !isTrackpad, let last = lastPinchLocation, g.numberOfTouches == lastPinchTouchCount {
                translation = CGSize(width: location.x - last.x, height: location.y - last.y)
            }
            lastPinchLocation = location
            lastPinchTouchCount = g.numberOfTouches

            onTransform(translation, factor, anchor(location, in: view))

        default:
            lastPinchLocation = nil
            lastPinchTouchCount = 0
            lastPinchScale = 1
        }
    }

    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer,
                           shouldRecognizeSimultaneouslyWith other: UIGestureRecognizer) -> Bool {
        true
    }
}
