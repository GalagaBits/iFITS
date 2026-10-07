//
//  SharePresenter.swift
//  iFITS Start
//
//  Shows the system share sheet from a SwiftUI view, pointing at a given spot (an iPad share
//  sheet is a popover and needs something to point at), plus the "Export and Send…" action.
//

import SwiftUI
import UIKit

/// Presents share sheets from wherever its ShareAnchor sits.
@MainActor
final class SharePresenter {
    /// Set by ShareAnchor.
    weak var anchor: UIView?

    /// Shows the share sheet for `items` (files, images, …).
    /// - activities: extra actions shown in the sheet (e.g. Export and Send).
    /// - completion: true if something was shared or saved.
    func share(_ items: [Any], activities: [UIActivity] = [], completion: ((Bool) -> Void)? = nil) {
        guard let anchor, let window = anchor.window, var top = window.rootViewController else { return }
        while let presented = top.presentedViewController, !presented.isBeingDismissed { top = presented }
        let sheet = UIActivityViewController(activityItems: items, applicationActivities: activities)
        sheet.completionWithItemsHandler = { _, completed, _, _ in completion?(completed) }
        if let popover = sheet.popoverPresentationController {
            popover.sourceView = anchor
            popover.sourceRect = anchor.bounds
            popover.permittedArrowDirections = [.up, .down]
        }
        top.present(sheet, animated: true)
    }
}

/// An invisible view that marks where share sheets point from. Put it behind the share button.
struct ShareAnchor: UIViewRepresentable {
    let presenter: SharePresenter

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        view.backgroundColor = .clear
        view.isUserInteractionEnabled = false
        presenter.anchor = view
        return view
    }

    func updateUIView(_ view: UIView, context: Context) {
        presenter.anchor = view
    }
}

/// "Export and Send…" in the share sheet: closes it, then opens the export choices.
final class ExportAndSendActivity: UIActivity {
    private let action: () -> Void

    init(action: @escaping () -> Void) {
        self.action = action
        super.init()
    }

    override class var activityCategory: UIActivity.Category { .action }
    override var activityType: UIActivity.ActivityType? { UIActivity.ActivityType("iFITS.exportAndSend") }
    override var activityTitle: String? { "Export and Send…" }
    override var activityImage: UIImage? { UIImage(systemName: "photo.badge.arrow.down") }
    override func canPerform(withActivityItems activityItems: [Any]) -> Bool { true }

    override func perform() {
        activityDidFinish(false)
        let action = self.action
        // After the share sheet has gone away.
        Task { @MainActor in
            try? await Task.sleep(for: .milliseconds(450))
            action()
        }
    }
}
