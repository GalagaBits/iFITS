//
//  ContentView+Header.swift
//  iFITS Start
//
//  Opening the header window.
//

import SwiftUI
import UIKit

extension ContentView {
    /// Opens the header in its own window, or as a sheet where that isn't possible (see `opensExtraWindows`).
    func showHeader() {
        guard !headerCards.isEmpty else { return }
        let document = FITSHeaderDocument(id: UUID(), fileName: fileName, cards: headerCards)
        if opensExtraWindows {
            openWindow(id: "fits-header", value: document)
        } else {
            headerSheetDocument = document
        }
    }

    /// Whether the Header and Spectra open in their own windows:
    /// - iPad: always.
    /// - iPhone Duo, open (its inner display is regular width and height): yes, so they can sit
    ///   next to the image in Split View.
    /// - iPhone Duo folded, and other iPhones: no, they open as sheets. These can't open extra windows.
    var opensExtraWindows: Bool {
        guard UIApplication.shared.supportsMultipleScenes else { return false }
        if UIDevice.current.userInterfaceIdiom == .pad { return true }
        return horizontalSizeClass == .regular && verticalSizeClass == .regular
    }
}
