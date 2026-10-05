//
//  ARVolumeView.swift
//  iFITS Start
//
//  The Metal view that shows the cube, with its gestures: drag to rotate, pinch or scroll to
//  zoom, twist to roll, double-tap to reset, and (camera mode) tap a surface to place the cube.
//

import SwiftUI
import MetalKit

struct ARVolumeView: UIViewRepresentable {
    let volume: ARVolume
    let axes: ARAxisSet?
    let settings: ARRenderSettings
    var onLabels: ([ARScreenLabel]) -> Void
    var onStatus: (String?) -> Void
    var onRoomFailed: () -> Void
    /// The view can't draw (no Metal, or the shaders are missing from the app).
    var onUnavailable: () -> Void

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> MTKView {
        let view = MTKView(frame: .zero, device: MTLCreateSystemDefaultDevice())
        view.backgroundColor = .black
        let c = context.coordinator
        guard let renderer = ARVolumeRenderer(view: view) else {
            DispatchQueue.main.async { onUnavailable() }
            return view
        }
        c.renderer = renderer
        hookUp(renderer)
        renderer.setVolume(volume)
        renderer.setAxes(axes)
        renderer.apply(settings)
        c.lastSettings = settings

        // Gestures
        let rotate = UIPanGestureRecognizer(target: c, action: #selector(Coordinator.rotate(_:)))
        rotate.maximumNumberOfTouches = 1
        let scroll = UIPanGestureRecognizer(target: c, action: #selector(Coordinator.scroll(_:)))
        scroll.allowedScrollTypesMask = .all          // trackpad / mouse wheel: zoom
        scroll.maximumNumberOfTouches = 0
        let pinch = UIPinchGestureRecognizer(target: c, action: #selector(Coordinator.pinch(_:)))
        let twist = UIRotationGestureRecognizer(target: c, action: #selector(Coordinator.twist(_:)))
        let doubleTap = UITapGestureRecognizer(target: c, action: #selector(Coordinator.doubleTap(_:)))
        doubleTap.numberOfTapsRequired = 2
        let tap = UITapGestureRecognizer(target: c, action: #selector(Coordinator.tap(_:)))
        tap.require(toFail: doubleTap)
        for g in [rotate, scroll, pinch, twist, doubleTap, tap] as [UIGestureRecognizer] {
            g.delegate = c
            view.addGestureRecognizer(g)
        }
        return view
    }

    func updateUIView(_ view: MTKView, context: Context) {
        let c = context.coordinator
        guard let renderer = c.renderer else { return }
        hookUp(renderer)
        // Only real changes redraw (label updates also come through here).
        guard c.lastSettings != settings else { return }
        c.lastSettings = settings
        renderer.apply(settings)
    }

    static func dismantleUIView(_ view: MTKView, coordinator: Coordinator) {
        coordinator.renderer?.stop()
    }

    private func hookUp(_ renderer: ARVolumeRenderer) {
        // Labels arrive after drawing; status and failures can come from inside a SwiftUI update
        // (apply), so they're passed on a moment later.
        renderer.onLabels = onLabels
        let status = onStatus, failed = onRoomFailed
        renderer.onStatus = { text in DispatchQueue.main.async { status(text) } }
        renderer.onRoomFailed = { DispatchQueue.main.async { failed() } }
    }

    final class Coordinator: NSObject, UIGestureRecognizerDelegate {
        var renderer: ARVolumeRenderer?
        var lastSettings: ARRenderSettings?

        /// Draws faster (at 1×) while a gesture is in progress.
        private func track(_ g: UIGestureRecognizer) {
            switch g.state {
            case .began: renderer?.setInteracting(true)
            case .ended, .cancelled, .failed: renderer?.setInteracting(false)
            default: break
            }
        }

        @objc func rotate(_ g: UIPanGestureRecognizer) {
            track(g)
            let t = g.translation(in: g.view)
            g.setTranslation(.zero, in: g.view)
            renderer?.rotate(dx: Float(t.x), dy: Float(t.y))
        }

        @objc func scroll(_ g: UIPanGestureRecognizer) {
            track(g)
            let t = g.translation(in: g.view)
            g.setTranslation(.zero, in: g.view)
            renderer?.zoom(by: Float(exp(-t.y * 0.004)))
        }

        @objc func pinch(_ g: UIPinchGestureRecognizer) {
            track(g)
            renderer?.zoom(by: Float(g.scale))
            g.scale = 1
        }

        @objc func twist(_ g: UIRotationGestureRecognizer) {
            track(g)
            renderer?.twist(by: Float(g.rotation))
            g.rotation = 0
        }

        @objc func doubleTap(_ g: UITapGestureRecognizer) {
            renderer?.resetView()
        }

        @objc func tap(_ g: UITapGestureRecognizer) {
            guard let renderer, renderer.inRoom else { return }
            renderer.place(at: g.location(in: g.view))
        }

        // Pinch and twist work together.
        func gestureRecognizer(_ g: UIGestureRecognizer,
                               shouldRecognizeSimultaneouslyWith other: UIGestureRecognizer) -> Bool {
            (g is UIPinchGestureRecognizer && other is UIRotationGestureRecognizer)
                || (g is UIRotationGestureRecognizer && other is UIPinchGestureRecognizer)
        }
    }
}
