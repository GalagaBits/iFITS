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
    }
}
