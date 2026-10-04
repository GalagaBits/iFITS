//
//  ContentView+Header.swift
//  iFITS Start
//
//  Opening the header window.
//

import SwiftUI
import UIKit

extension ContentView {
    /// Opens the header in its own window (iPadOS), or as a sheet if extra windows aren't available.
    func showHeader() {
        guard !headerCards.isEmpty else { return }
        let document = FITSHeaderDocument(id: UUID(), fileName: fileName, cards: headerCards)
        if UIApplication.shared.supportsMultipleScenes {
            openWindow(id: "fits-header", value: document)
        } else {
            headerSheetDocument = document
        }
    }
}
