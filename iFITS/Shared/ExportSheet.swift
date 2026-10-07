//
//  ExportSheet.swift
//  iFITS Start
//
//  "Export and Send", like Pages: choose a format (PNG or JPEG), see the result, then Share.
//  Used for the image (main window), spectra (dock and Spectra window) and the AR view.
//

import SwiftUI
import UIKit

enum ExportFormat: String, CaseIterable, Identifiable {
    case png, jpeg

    var id: Self { self }
    var title: String { self == .png ? "PNG" : "JPEG" }
    var detail: String {
        self == .png ? "Lossless; best for plots and figures." : "Smaller file; best for sending quickly."
    }
    var fileExtension: String { self == .png ? "png" : "jpg" }

    func encode(_ image: UIImage) -> Data? {
        self == .png ? image.pngData() : image.jpegData(compressionQuality: 0.92)
    }
}

/// What to export: a name for the file, and how to draw the picture (white background).
struct ExportRequest: Identifiable {
    let id = UUID()
    /// "Export Your Image", "Export Spectra", …
    let title: String
    /// File name without extension.
    let baseName: String
    /// Draws the picture (on the main thread).
    let render: @MainActor () -> UIImage?
}

struct ExportSheet: View {
    let request: ExportRequest
    var onClose: () -> Void

    @State private var format: ExportFormat?
    @State private var image: UIImage?
    @State private var file: URL?
    @State private var failed = false
    @State private var sharer = SharePresenter()

    var body: some View {
        NavigationStack {
            Group {
                if let format {
                    exportPage(format)
                } else {
                    formatList
                }
            }
            .navigationTitle(format == nil ? request.title : "Export")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button(action: onClose) {
                        Image(systemName: "xmark")
                    }
                    .keyboardShortcut(.cancelAction)
                    .accessibilityLabel("Close")
                }
                if format != nil {
                    ToolbarItem(placement: .topBarLeading) {
                        Button {
                            withAnimation(.snappy) { reset() }
                        } label: {
                            Image(systemName: "chevron.backward")
                        }
                        .accessibilityLabel("Choose another format")
                    }
                }
            }
        }
        .tint(.orange)
    }

    // MARK: Pages

    private var formatList: some View {
        List {
            Section {
                ForEach(ExportFormat.allCases) { f in
                    Button {
                        choose(f)
                    } label: {
                        HStack {
                            VStack(alignment: .leading, spacing: 2) {
                                Text(f.title)
                                    .foregroundStyle(.primary)
                                Text(f.detail)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                            }
                            Spacer()
                            Image(systemName: "chevron.forward")
                                .font(.footnote.weight(.semibold))
                                .foregroundStyle(.tertiary)
                        }
                        .contentShape(Rectangle())
                    }
                    .buttonStyle(.plain)
                }
            } header: {
                Text("Choose a format.")
                    .textCase(nil)
            }
        }
    }

    private func exportPage(_ format: ExportFormat) -> some View {
        VStack(spacing: 18) {
            Spacer(minLength: 12)
            if let image {
                Image(uiImage: image)
                    .resizable()
                    .interpolation(.high)
                    .aspectRatio(contentMode: .fit)
                    .frame(maxWidth: 320, maxHeight: 260)
                    .shadow(color: .black.opacity(0.25), radius: 8, y: 2)
            } else if failed {
                Label("Couldn't make the \(format.title).", systemImage: "exclamationmark.triangle")
                    .foregroundStyle(.secondary)
            } else {
                ProgressView()
                    .frame(height: 200)
            }
            Text(file?.lastPathComponent ?? "\(request.baseName).\(format.fileExtension)")
                .font(.headline)
                .lineLimit(2)
                .multilineTextAlignment(.center)
            Button {
                guard let file else { return }
                sharer.share([file]) { completed in
                    if completed { onClose() }
                }
            } label: {
                Text("Share")
                    .font(.headline)
                    .frame(maxWidth: 240)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .disabled(file == nil)
            .background(ShareAnchor(presenter: sharer))
            Spacer(minLength: 12)
        }
        .padding(24)
        .frame(maxWidth: .infinity)
    }

    // MARK: Making the file

    private func choose(_ f: ExportFormat) {
        withAnimation(.snappy) { format = f }
        failed = false
        // Draw on the next turn of the run loop, so the page appears first.
        Task { @MainActor in
            guard let picture = request.render(), let data = f.encode(picture) else {
                failed = true
                return
            }
            let folder = FileManager.default.temporaryDirectory
                .appendingPathComponent("Export-\(UUID().uuidString)", isDirectory: true)
            let url = folder.appendingPathComponent("\(Self.safeName(request.baseName)).\(f.fileExtension)")
            do {
                try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
                try data.write(to: url, options: .atomic)
                image = picture
                file = url
            } catch {
                failed = true
            }
        }
    }

    private func reset() {
        format = nil
        image = nil
        file = nil
        failed = false
    }

    /// A file name without characters that can't be in one.
    private static func safeName(_ name: String) -> String {
        let cleaned = name.replacingOccurrences(of: "/", with: "-").replacingOccurrences(of: ":", with: "-")
        return cleaned.isEmpty ? "iFITS Export" : cleaned
    }
}

/// White-background picture helpers for exports.
enum ExportImage {
    /// Draws `image` over white (transparent pixels become white), without smoothing pixels.
    static func onWhite(_ image: UIImage, size: CGSize, overlays: [UIImage] = []) -> UIImage {
        let format = UIGraphicsImageRendererFormat()
        format.scale = 1
        format.opaque = true
        return UIGraphicsImageRenderer(size: size, format: format).image { ctx in
            let rect = CGRect(origin: .zero, size: size)
            UIColor.white.setFill()
            ctx.fill(rect)
            ctx.cgContext.interpolationQuality = .none
            image.draw(in: rect)
            ctx.cgContext.interpolationQuality = .high
            for overlay in overlays { overlay.draw(in: rect) }
        }
    }
}
