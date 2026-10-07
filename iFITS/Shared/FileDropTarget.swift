//
//  FileDropTarget.swift
//  iFITS Start
//
//  Drag a FITS file from Files (or Mail, Safari's downloads, …) anywhere onto the window to open it.
//  The drop interaction goes on the window itself, so a drop lands even over the image, whose UIKit
//  gesture views would otherwise keep it from reaching SwiftUI.
//

import SwiftUI
import UIKit
import UniformTypeIdentifiers

struct FileDropTarget: UIViewRepresentable {
    /// Off while something covers the window (e.g. the AR view).
    var isEnabled = true
    /// A FITS file is being dragged over the window (true), or has left / been dropped (false).
    var onTargeted: (Bool) -> Void
    /// The dropped file: in place when the app it came from allows that (so Save writes to it),
    /// otherwise a copy in iFITS's Documents › Dropped Files folder.
    var onDrop: (URL) -> Void

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> InstallerView {
        context.coordinator.target = self
        let view = InstallerView()
        view.coordinator = context.coordinator
        view.isUserInteractionEnabled = false
        return view
    }

    func updateUIView(_ view: InstallerView, context: Context) {
        context.coordinator.target = self
    }

    static func dismantleUIView(_ view: InstallerView, coordinator: Coordinator) {
        coordinator.uninstall()
    }

    /// An invisible view that puts the drop interaction on its window.
    final class InstallerView: UIView {
        weak var coordinator: Coordinator?

        override func didMoveToWindow() {
            super.didMoveToWindow()
            coordinator?.install(on: window)
        }
    }

    final class Coordinator: NSObject, UIDropInteractionDelegate {
        var target: FileDropTarget?
        private var interaction: UIDropInteraction?
        private weak var host: UIView?

        func install(on window: UIWindow?) {
            guard let window else { return }
            guard host !== window else { return }
            uninstall()
            let interaction = UIDropInteraction(delegate: self)
            window.addInteraction(interaction)
            self.interaction = interaction
            host = window
        }

        func uninstall() {
            if let interaction, let host {
                host.removeInteraction(interaction)
            }
            interaction = nil
            host = nil
        }

        // MARK: UIDropInteractionDelegate

        func dropInteraction(_ interaction: UIDropInteraction, canHandle session: UIDropSession) -> Bool {
            guard target?.isEnabled == true else { return false }
            return session.items.contains { FileDropTarget.isFITS($0.itemProvider) }
        }

        func dropInteraction(_ interaction: UIDropInteraction,
                             sessionDidUpdate session: UIDropSession) -> UIDropProposal {
            UIDropProposal(operation: target?.isEnabled == true ? .copy : .forbidden)
        }

        func dropInteraction(_ interaction: UIDropInteraction, sessionDidEnter session: UIDropSession) {
            target?.onTargeted(true)
        }

        func dropInteraction(_ interaction: UIDropInteraction, sessionDidExit session: UIDropSession) {
            target?.onTargeted(false)
        }

        func dropInteraction(_ interaction: UIDropInteraction, sessionDidEnd session: UIDropSession) {
            target?.onTargeted(false)
        }

        func dropInteraction(_ interaction: UIDropInteraction, performDrop session: UIDropSession) {
            target?.onTargeted(false)
            // The first FITS file (iFITS shows one image per window).
            guard let provider = session.items.map(\.itemProvider).first(where: FileDropTarget.isFITS) else {
                return
            }
            FileDropTarget.loadFile(from: provider) { [weak self] url in
                guard let url else { return }
                self?.target?.onDrop(url)
            }
        }
    }

    // MARK: Files

    /// Whether a dragged item is a FITS file (by name or by type).
    nonisolated static func isFITS(_ provider: NSItemProvider) -> Bool {
        if let name = provider.suggestedName, FITSFileName.matches(name) { return true }
        return provider.registeredTypeIdentifiers.contains { identifier in
            guard let type = UTType(identifier) else { return false }
            if type.conforms(to: .fitsFile) { return true }
            let extensions = type.tags[.filenameExtension] ?? []
            return extensions.contains { FITSFileName.extensions.contains($0.lowercased()) }
        }
    }

    /// Gets the dragged file: in place if allowed, otherwise copied into Documents › Dropped Files
    /// (the system deletes its temporary copy as soon as the loading finishes).
    nonisolated static func loadFile(from provider: NSItemProvider,
                                     completion: @escaping @MainActor @Sendable (URL?) -> Void) {
        // The FITS type first; otherwise any file data that isn't a link or a preview.
        let identifiers = provider.registeredTypeIdentifiers
        let fitsIdentifier = identifiers.first { identifier in
            guard let type = UTType(identifier) else { return false }
            return type.conforms(to: .fitsFile)
                || (type.tags[.filenameExtension] ?? []).contains { FITSFileName.extensions.contains($0.lowercased()) }
        }
        let identifier = fitsIdentifier
            ?? identifiers.first { identifier in
                guard let type = UTType(identifier) else { return false }
                return type.conforms(to: .data) && !type.conforms(to: .url) && !type.conforms(to: .image)
            }
            ?? identifiers.first ?? UTType.data.identifier
        let suggestedName = provider.suggestedName

        provider.loadInPlaceFileRepresentation(forTypeIdentifier: identifier) { url, inPlace, _ in
            var result: URL?
            if let url {
                result = inPlace ? url : keepCopy(of: url, suggestedName: suggestedName)
            }
            let opened = result
            Task { @MainActor in completion(opened) }
        }
    }

    /// `name` in `folder`, or "name 2", "name 3", … if that's taken.
    nonisolated private static func uniqueURL(in folder: URL, name: String) -> URL {
        let base = (name as NSString).deletingPathExtension
        let ext = (name as NSString).pathExtension
        var destination = folder.appendingPathComponent(name)
        var n = 2
        while FileManager.default.fileExists(atPath: destination.path) {
            destination = folder.appendingPathComponent(ext.isEmpty ? "\(base) \(n)" : "\(base) \(n).\(ext)")
            n += 1
        }
        return destination
    }

    /// Copies a dropped file into Documents › Dropped Files (a new name if one is already there).
    nonisolated private static func keepCopy(of url: URL, suggestedName: String?) -> URL? {
        let manager = FileManager.default
        guard let documents = manager.urls(for: .documentDirectory, in: .userDomainMask).first else { return nil }
        let folder = documents.appendingPathComponent("Dropped Files", isDirectory: true)
        do {
            try manager.createDirectory(at: folder, withIntermediateDirectories: true)
            var name = url.lastPathComponent
            if !FITSFileName.matches(name) {
                name = (suggestedName ?? "Dropped File") + ".fits"
                if let suggestedName, FITSFileName.matches(suggestedName) { name = suggestedName }
            }
            let destination = uniqueURL(in: folder, name: name)
            let scoped = url.startAccessingSecurityScopedResource()
            defer { if scoped { url.stopAccessingSecurityScopedResource() } }
            try manager.copyItem(at: url, to: destination)
            return destination
        } catch {
            return nil
        }
    }
}
