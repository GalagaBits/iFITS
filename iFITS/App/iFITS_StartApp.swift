//
//  iFITS_StartApp.swift
//  iFITS Start
//
//  Created by Derod Deal on 4/14/26.
//

import SwiftUI
import Combine

@main
struct iFITS_StartApp: App {
    /// Connects the menu bar to the main window (see Menu/FITSCommandCenter.swift).
    @StateObject private var commandCenter = FITSCommandCenter()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(commandCenter)
                // A FITS file opened from the Files app loads into an open iFITS window (the
                // frontmost one) instead of a new window each time.
                // (Only file URLs are preferred, so the Header and Spectra windows still open as
                // their own windows.)
                .handlesExternalEvents(preferring: ["file:"], allowing: ["*"])
        }
        // Menu bar: File / View / Mode / Visualization / Annotate (see FITSMenuCommands).
        .commands {
            FITSMenuCommands(center: commandCenter)
        }

        // Separate window for the FITS header, opened by the "H" button.
        WindowGroup("FITS Header", id: "fits-header", for: FITSHeaderDocument.self) { $document in
            if let document {
                HeaderWindowView(document: document)
            }
        }

        // Separate window for a cube's spectra ("<file> — Spectra"), opened from the Spectra dock.
        // It shares the main window's spectrum (see SpectraWindowLink).
        WindowGroup("Spectra", id: "spectra", for: UUID.self) { $linkID in
            SpectraWindowView(link: SpectraWindowLink.link(linkID))
        }
    }
}
