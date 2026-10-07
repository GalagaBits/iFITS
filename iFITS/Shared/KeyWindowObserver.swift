//
//  KeyWindowObserver.swift
//  iFITS Start
//
//  Tells a window when it becomes the key window: the frontmost one, which the keyboard and the
//  menu bar act on. The main window uses it to point the menu bar at itself, so menu items keep
//  working with several iFITS windows open, or after one is closed.
//

import SwiftUI
import UIKit

struct KeyWindowObserver: UIViewRepresentable {
    /// Called when this view's window becomes key (and once at the start if it already is).
    var onBecomeKey: () -> Void

    func makeUIView(context: Context) -> ObserverView {
        let view = ObserverView()
        view.onBecomeKey = onBecomeKey
        view.isUserInteractionEnabled = false
        return view
    }

    func updateUIView(_ view: ObserverView, context: Context) {
        view.onBecomeKey = onBecomeKey
    }

    /// An invisible view that watches its own window.
    final class ObserverView: UIView {
        var onBecomeKey: (() -> Void)?
        nonisolated(unsafe) private var observer: NSObjectProtocol?

        override func didMoveToWindow() {
            super.didMoveToWindow()
            if let observer {
                NotificationCenter.default.removeObserver(observer)
                self.observer = nil
            }
            guard let window else { return }
            observer = NotificationCenter.default.addObserver(forName: UIWindow.didBecomeKeyNotification,
                                                              object: window, queue: .main) { [weak self] _ in
                MainActor.assumeIsolated { self?.onBecomeKey?() }
            }
            if window.isKeyWindow {
                DispatchQueue.main.async { [weak self] in self?.onBecomeKey?() }
            }
        }

        deinit {
            if let observer { NotificationCenter.default.removeObserver(observer) }
        }
    }
}
