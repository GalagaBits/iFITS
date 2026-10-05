//
//  SpectrumGestureView.swift
//  iFITS Start
//
//  UIKit gestures for the spectrum graph (SwiftUI can't tell a two-finger drag or a trackpad
//  scroll from other gestures):
//  - one finger / click-drag → drag the orange line (or pan, when zoomed)
//  - tap → move the line there; double tap → zoom back out
//  - pinch (fingers or trackpad) → zoom around the fingers
//  - two fingers up / down (touch screen or trackpad) → zoom around where they started
//  - two fingers left / right (touch screen or trackpad) → scroll along the spectrum
//  - pointer / Apple Pencil hover → a faint line where it is
//

import SwiftUI
import UIKit

enum SpectrumGesturePhase {
    case began, changed, ended
    /// Another gesture took over: undo what this one did.
    case cancelled
}

struct SpectrumGestureView: UIViewRepresentable {
    var onTap: (CGPoint) -> Void
    var onDoubleTap: (CGPoint) -> Void
    /// One finger or click-drag: (phase, where it started, where it is now).
    var onDrag: (SpectrumGesturePhase, CGPoint, CGPoint) -> Void
    /// Zoom: (phase, anchor point, total zoom factor since it began; > 1 = zoom in).
    var onZoom: (SpectrumGesturePhase, CGPoint, Double) -> Void
    /// Scroll: (phase, total horizontal movement since it began, in points).
    var onScroll: (SpectrumGesturePhase, CGFloat) -> Void
    /// The trackpad / mouse pointer or a hovering Apple Pencil: where it is, or nil once it leaves.
    var onHover: (CGPoint?) -> Void = { _ in }

    func makeCoordinator() -> SpectrumGestureCoordinator { SpectrumGestureCoordinator() }

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        view.backgroundColor = .clear
        view.isAccessibilityElement = false
        context.coordinator.install(on: view)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        let c = context.coordinator
        c.onTap = onTap
        c.onDoubleTap = onDoubleTap
        c.onDrag = onDrag
        c.onZoom = onZoom
        c.onScroll = onScroll
        c.onHover = onHover
    }

    static func dismantleUIView(_ uiView: UIView, coordinator: SpectrumGestureCoordinator) {
        for g in uiView.gestureRecognizers ?? [] { uiView.removeGestureRecognizer(g) }
    }
}

final class SpectrumGestureCoordinator: NSObject, UIGestureRecognizerDelegate {
    var onTap: (CGPoint) -> Void = { _ in }
    var onDoubleTap: (CGPoint) -> Void = { _ in }
    var onDrag: (SpectrumGesturePhase, CGPoint, CGPoint) -> Void = { _, _, _ in }
    var onZoom: (SpectrumGesturePhase, CGPoint, Double) -> Void = { _, _, _ in }
    var onScroll: (SpectrumGesturePhase, CGFloat) -> Void = { _, _ in }
    var onHover: (CGPoint?) -> Void = { _ in }

    /// Zoom speed for two-finger up / down movement: the factor per point moved.
    private let zoomPerPoint = 0.01
    /// Movement (points) before a two-finger gesture picks zoom (up / down) or scroll (left / right).
    private let axisLockDistance: CGFloat = 8

    private enum TwoFingerAxis { case undecided, zoom, scroll, ignored }

    private var dragActive = false
    private var pinchActive = false
    /// A pinch has begun but hasn't spread / closed enough to count yet (two-finger swipes always
    /// change the finger gap a little, so a pinch only takes over after a clear change of scale).
    private var pinchCandidate = false
    private var pinchAnchor: CGPoint = .zero
    private var pinchScaleAtStart: CGFloat = 1
    /// Scale change (log) that makes a pinch a pinch.
    private let pinchThreshold = 0.08
    /// The two-finger touch drag and the trackpad scroll, keyed by recognizer.
    private var twoFinger: [ObjectIdentifier: (axis: TwoFingerAxis, start: CGPoint, lock: CGPoint)] = [:]

    func install(on view: UIView) {
        let drag = UIPanGestureRecognizer(target: self, action: #selector(handleDrag(_:)))
        drag.maximumNumberOfTouches = 1

        let twoFingers = UIPanGestureRecognizer(target: self, action: #selector(handleTwoFinger(_:)))
        twoFingers.minimumNumberOfTouches = 2
        twoFingers.maximumNumberOfTouches = 2
        twoFingers.allowedScrollTypesMask = []

        // Two-finger trackpad scroll or mouse wheel (no touches).
        let scroll = UIPanGestureRecognizer(target: self, action: #selector(handleTwoFinger(_:)))
        scroll.allowedScrollTypesMask = .all
        scroll.allowedTouchTypes = []

        let pinch = UIPinchGestureRecognizer(target: self, action: #selector(handlePinch(_:)))

        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap(_:)))
        let doubleTap = UITapGestureRecognizer(target: self, action: #selector(handleDoubleTap(_:)))
        doubleTap.numberOfTapsRequired = 2

        // Pointer and Apple Pencil hover (iPadOS reports both through this recognizer).
        let hover = UIHoverGestureRecognizer(target: self, action: #selector(handleHover(_:)))

        for g in [drag, twoFingers, scroll, pinch, tap, doubleTap, hover] as [UIGestureRecognizer] {
            g.delegate = self
            view.addGestureRecognizer(g)
        }
    }

    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer,
                           shouldRecognizeSimultaneouslyWith other: UIGestureRecognizer) -> Bool {
        true
    }

    // MARK: Hover

    @objc func handleHover(_ g: UIHoverGestureRecognizer) {
        guard let view = g.view else { return }
        switch g.state {
        case .began, .changed: onHover(g.location(in: view))
        default: onHover(nil)
        }
    }

    // MARK: Taps

    @objc func handleTap(_ g: UITapGestureRecognizer) {
        guard g.state == .ended, let view = g.view else { return }
        onTap(g.location(in: view))
    }

    @objc func handleDoubleTap(_ g: UITapGestureRecognizer) {
        guard g.state == .ended, let view = g.view else { return }
        onDoubleTap(g.location(in: view))
    }

    // MARK: One finger

    @objc func handleDrag(_ g: UIPanGestureRecognizer) {
        guard let view = g.view else { return }
        let location = g.location(in: view)
        switch g.state {
        case .began:
            // A second finger is already down for a pinch or two-finger drag: leave it to them.
            guard !pinchActive, !twoFinger.values.contains(where: { $0.axis != .ignored }) else { return }
            let t = g.translation(in: view)
            dragActive = true
            onDrag(.began, CGPoint(x: location.x - t.x, y: location.y - t.y), location)
        case .changed:
            guard dragActive else { return }
            onDrag(.changed, .zero, location)
        case .ended:
            guard dragActive else { return }
            dragActive = false
            onDrag(.ended, .zero, location)
        default:
            guard dragActive else { return }
            dragActive = false
            onDrag(.cancelled, .zero, location)
        }
    }

    /// A two-finger gesture started: the one-finger drag (its first finger) is undone.
    private func cancelDrag() {
        guard dragActive else { return }
        dragActive = false
        onDrag(.cancelled, .zero, .zero)
    }

    // MARK: Pinch

    @objc func handlePinch(_ g: UIPinchGestureRecognizer) {
        guard let view = g.view else { return }
        switch g.state {
        case .began:
            pinchCandidate = true
            pinchAnchor = g.location(in: view)
        case .changed:
            if pinchCandidate, !pinchActive {
                // A two-finger swipe that already picked zoom or scroll keeps going instead.
                let swipeLocked = twoFinger.values.contains { $0.axis == .zoom || $0.axis == .scroll }
                guard !swipeLocked else { return }
                guard abs(log(Double(max(g.scale, 0.001)))) > pinchThreshold else { return }
                cancelDrag()
                endTwoFingerGestures()
                pinchActive = true
                pinchScaleAtStart = g.scale
                onZoom(.began, pinchAnchor, 1)
            }
            guard pinchActive else { return }
            onZoom(.changed, .zero, Double(g.scale / max(pinchScaleAtStart, 0.001)))
        default:
            pinchCandidate = false
            guard pinchActive else { return }
            pinchActive = false
            onZoom(.ended, .zero, 1)
        }
    }

    // MARK: Two fingers (touch) and trackpad scroll

    @objc func handleTwoFinger(_ g: UIPanGestureRecognizer) {
        guard let view = g.view else { return }
        let key = ObjectIdentifier(g)
        let t = g.translation(in: view)
        switch g.state {
        case .began:
            if g.numberOfTouches >= 2 { cancelDrag() }
            // While pinching, the pinch zooms.
            let start = g.location(in: view)
            let axis: TwoFingerAxis = pinchActive ? .ignored : .undecided
            twoFinger[key] = (axis: axis, start: start, lock: .zero)
        case .changed:
            guard var state = twoFinger[key] else { return }
            if pinchActive, state.axis != .ignored {
                finish(state.axis, translation: t, lock: state.lock)
                state.axis = .ignored
            }
            switch state.axis {
            case .undecided:
                guard hypot(t.x, t.y) >= axisLockDistance else { break }
                state.lock = t
                if abs(t.y) > abs(t.x) {
                    state.axis = .zoom
                    onZoom(.began, state.start, 1)
                } else {
                    state.axis = .scroll
                    onScroll(.began, 0)
                }
            case .zoom:
                // Fingers up = zoom in.
                onZoom(.changed, .zero, exp(-Double(t.y - state.lock.y) * zoomPerPoint))
            case .scroll:
                onScroll(.changed, t.x - state.lock.x)
            case .ignored:
                break
            }
            twoFinger[key] = state
        default:
            if let state = twoFinger[key] { finish(state.axis, translation: t, lock: state.lock) }
            twoFinger[key] = nil
        }
    }

    private func finish(_ axis: TwoFingerAxis, translation t: CGPoint, lock: CGPoint) {
        switch axis {
        case .zoom: onZoom(.ended, .zero, exp(-Double(t.y - lock.y) * zoomPerPoint))
        case .scroll: onScroll(.ended, t.x - lock.x)
        case .undecided, .ignored: break
        }
    }

    /// A pinch started: two-finger zooms / scrolls in progress stop where they are.
    private func endTwoFingerGestures() {
        for (key, state) in twoFinger {
            switch state.axis {
            case .zoom: onZoom(.ended, .zero, 1)
            case .scroll: onScroll(.ended, 0)
            case .undecided, .ignored: break
            }
            twoFinger[key] = (axis: TwoFingerAxis.ignored, start: state.start, lock: state.lock)
        }
    }
}
