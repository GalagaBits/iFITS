//
//  ContentView+Share.swift
//  iFITS Start
//
//  The share button (the FITS file, through the system share sheet), "Export and Send" a PNG or
//  JPEG of the image, and the Settings sheet.
//

import SwiftUI
import UIKit
import PencilKit

extension ContentView {
    // MARK: Share

    /// Share button: the FITS file in the share sheet, with "Export and Send…" for a PNG / JPEG.
    /// Unsaved annotations and regions are saved first when autosave is on.
    func shareFITS() {
        guard let url = loadedFileURL else { return }
        let present = {
            toolbarSharer.share([url], activities: [ExportAndSendActivity { exportImage() }])
        }
        if autosaveEnabled, hasUnsavedEdits, autosaveFailedURL != url {
            autosaveNow()
            waitForSave(then: present)
        } else {
            present()
        }
    }

    /// Runs `action` once the current save has finished (gives up waiting after 10 s).
    private func waitForSave(then action: @escaping () -> Void, waited: Double = 0) {
        if !isSaving || waited > 10 {
            action()
            return
        }
        Task { @MainActor in
            try? await Task.sleep(for: .milliseconds(100))
            waitForSave(then: action, waited: waited + 0.1)
        }
    }

    /// Opens "Export and Send" for the image as shown: colormap and scaling, annotations (if shown)
    /// and regions, on white.
    func exportImage() {
        guard fitsImage != nil else { return }
        let base = (fileName as NSString).deletingPathExtension
        let channel = cubeSource == nil ? "" : " ch\(animator.index(onAxis: 0))"
        exportRequest = ExportRequest(title: "Export Your Image", baseName: base + channel) {
            renderExportImage()
        }
    }

    /// The image at full resolution (at least 2048 pixels on the long side; pixels stay sharp),
    /// with annotations and regions, on a white background.
    func renderExportImage() -> UIImage? {
        guard let image = fitsImage, imageWidth > 0, imageHeight > 0 else { return nil }
        let w = imageWidth, h = imageHeight
        // Points across the long side, and the pixel scale on top of that.
        let points: CGFloat = 1024
        let perPixel = points / max(w, h)                     // points per image pixel
        let size = CGSize(width: w * perPixel, height: h * perPixel)
        // 2048–4096 pixels on the long side (bigger would use too much memory).
        let scale = min(max(2, ceil(max(w, h) / points)), 4)

        // Annotations, drawn by PencilKit at the export resolution (in light mode, so ink colours
        // stay as picked).
        var ink: UIImage?
        if annotations.isVisible, annotations.hasStrokes {
            let drawing = PKDrawing(strokes: annotations.strokes)
            UITraitCollection(userInterfaceStyle: .light).performAsCurrent {
                ink = drawing.image(from: CGRect(x: 0, y: 0, width: w, height: h), scale: perPixel * scale)
            }
        }
        let picture = ExportImage.onWhite(image, size: CGSize(width: size.width * scale, height: size.height * scale),
                                          overlays: ink.map { [$0] } ?? [])
        // Regions on top (same drawing as on screen).
        let content = ZStack {
            Image(uiImage: picture)
                .resizable()
                .frame(width: size.width, height: size.height)
            RegionOverlay(regions: regionStore.regions,
                          selectedID: nil,
                          showHandles: false,
                          imageToScreen: CGAffineTransform(scaleX: perPixel, y: perPixel),
                          imageHeight: h)
                .frame(width: size.width, height: size.height)
        }
        .environment(\.colorScheme, .light)
        let renderer = ImageRenderer(content: content)
        renderer.scale = scale
        renderer.isOpaque = true
        return renderer.uiImage ?? picture
    }

    // MARK: Hosts (sheets sit on their own empty views)

    /// Where the FITS share sheet points from: just under the toolbar's share button.
    var shareAnchorLayer: some View {
        ShareAnchor(presenter: toolbarSharer)
            .frame(width: 2, height: 2)
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topTrailing)
            .padding(.trailing, 112)
            .allowsHitTesting(false)
    }

    /// The "Export and Send" sheet.
    var exportHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .sheet(item: $exportRequest, onDismiss: { restoreImageFocus() }) { request in
                ExportSheet(request: request, onClose: { exportRequest = nil })
                    .presentationSizing(.form)
            }
    }

    /// The Settings sheet (••• menu).
    var settingsHost: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .sheet(isPresented: $showSettings, onDismiss: { restoreImageFocus() }) {
                SettingsSheet(autosave: $autosaveEnabled, onDone: { showSettings = false })
                    .presentationSizing(.form)
            }
    }
}

/// iFITS settings, like Pages': grouped switches, ✓ to close.
struct SettingsSheet: View {
    @Binding var autosave: Bool
    var onDone: () -> Void

    var body: some View {
        NavigationStack {
            Form {
                Section {
                    Toggle("Autosave", isOn: $autosave)
                } header: {
                    Text("Saving")
                } footer: {
                    Text("Annotations and regions are saved into the FITS file automatically as you work. Turn this off to save only with Save (⌘S).")
                }
            }
            .navigationTitle("Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button(action: onDone) {
                        Image(systemName: "checkmark")
                    }
                    .buttonStyle(.glassProminent)
                    .keyboardShortcut(.defaultAction)
                    .accessibilityLabel("Done")
                }
            }
        }
        .tint(.orange)
    }
}
