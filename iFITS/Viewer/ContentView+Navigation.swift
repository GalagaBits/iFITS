//
//  ContentView+Navigation.swift
//  iFITS Start
//
//  Zoom / pan math, the pixel inspector hookup, keyboard moves and focus.
//

import SwiftUI
import UIKit

extension ContentView {
    // MARK: - Zoom / Pan Math

    /// How many screen points one image pixel currently covers.
    func pointsPerImagePixel(in size: CGSize) -> CGFloat {
        guard imageWidth > 0, imageHeight > 0 else { return 0 }
        return scale * max(size.width / imageWidth, size.height / imageHeight)
    }

    /// Updates the pixel inspector for a screen point (ignored in A mode or off the image).
    func inspect(atScreen point: CGPoint, in size: CGSize, source: InspectSource) {
        guard selectedMode != "A", fitsImage != nil else { return }
        let q = point.applying(imageToScreenTransform(in: size).inverted())
        let column = Int(floor(q.x)), row = Int(floor(q.y))
        guard column >= 0, row >= 0, column < Int(imageWidth), row < Int(imageHeight) else { return }
        let pixel = InspectedPixel(column: column, row: row, source: source)
        guard pixel != inspectedPixel else { return }      // only redraw when it changes
        if inspectedPixel == nil {
            withAnimation(.snappy) { inspectedPixel = pixel }
        } else {
            inspectedPixel = pixel
        }
    }

    /// Image pixel (0…width, 0…height) → screen point, identical to how the image is drawn:
    /// aspect-fill in the viewport, scaled about the center, then offset.
    func imageToScreenTransform(in size: CGSize) -> CGAffineTransform {
        guard imageWidth > 0, imageHeight > 0, size.width > 0, size.height > 0 else { return .identity }
        let k = scale * max(size.width / imageWidth, size.height / imageHeight)
        return CGAffineTransform(a: k, b: 0, c: 0, d: k,
                                 tx: size.width / 2 + offset.width - k * imageWidth / 2,
                                 ty: size.height / 2 + offset.height - k * imageHeight / 2)
    }

    /// Pans by `translation`, then zooms by `scaleFactor` around `anchor`.
    /// `anchor` is measured from the center of the viewport (in points).
    func applyTransform(translation: CGSize = .zero, scaleFactor: CGFloat = 1, anchor: CGPoint = .zero) {
        var newOffset = CGSize(width: offset.width + translation.width,
                               height: offset.height + translation.height)

        let newScale = min(max(scale * scaleFactor, minScale), maxScale)
        let f = newScale / scale

        // Keep the point under the anchor fixed on screen while zooming.
        newOffset.width  = newOffset.width  * f + anchor.x * (1 - f)
        newOffset.height = newOffset.height * f + anchor.y * (1 - f)

        scale = newScale
        offset = newOffset
    }

    // MARK: - Reset

    func resetView() {
        withAnimation(viewChangeAnimation) {
            scale = 1.0
            offset = .zero
        }
    }

    // MARK: - Keyboard Support (Magic Keyboard / any hardware keyboard)
    // Shortcuts live in the menu bar (FITSMenuCommands); hold ⌘ to see them. The image area
    // also handles the arrow keys, and Esc / Delete for regions in R mode, while it has focus.

    /// Moves the view like scrolling a document: → reveals more of the right side, ↑ more of the top.
    /// `dx` / `dy` are −1, 0 or 1; `large` (Shift) moves 4× as far.
    func moveView(dx: CGFloat, dy: CGFloat, large: Bool) {
        guard fitsImage != nil else { return }
        let step = keyboardPanStep * (large ? 4 : 1)
        applyTransform(translation: CGSize(width: -dx * step, height: -dy * step))
    }

    // MARK: - Keyboard focus

    /// Gives keyboard focus back to the image area.
    ///
    /// Why this matters: iPadOS fills the menu bar (and runs keyboard shortcuts) by asking the
    /// focused part of the window which commands it handles. After the file picker or save dialog
    /// closes, or after the PencilKit canvas gives up focus when leaving A mode, nothing in the
    /// window has focus, so every SwiftUI menu shows "No Menu Items" and shortcuts stop working
    /// until the app is hidden and reopened. Re-focusing the image fixes that.
    /// (Skipped in A mode, where the PencilKit canvas needs focus for its tool palette.)
    func restoreImageFocus() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            guard selectedMode != "A" else { return }
            imageFocused = true
        }
    }

    /// Ends editing in any text box (e.g. Clip min / Clip max), so it stops taking the arrow keys.
    func endTextEditing() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }
}
