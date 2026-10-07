//
//  WindowLayout.swift
//  iFITS Start
//
//  How much room the window has. Small windows (Stage Manager, Split View, the smaller iPads) get an
//  iPhone-style "compact" layout: the V A R S buttons and the toolbar buttons fold into one » menu,
//  the statistics / spectrum / animator widgets step aside (and come back when the window grows),
//  Pixel Info shrinks to one line, and the bottom docks are sized to fit.
//

import SwiftUI

struct WindowLayout: Equatable {
    /// The window's area under the toolbar (keyboard ignored), in points.
    var size: CGSize = .zero

    /// Narrower than this: compact layout. (About what the toolbar's buttons and the file name
    /// need in one row; half of a 13" iPad's screen is 688 points.)
    static let compactWidth: CGFloat = 720
    /// Shorter than this (under the toolbar): compact layout. The V A R S column alone needs ~505.
    static let compactHeight: CGFloat = 520

    /// iPhone-style layout.
    var isCompact: Bool {
        guard size.width > 0, size.height > 0 else { return false }
        return size.width < Self.compactWidth || size.height < Self.compactHeight
    }

    /// Toolbar buttons (and the V A R S modes) in one » menu.
    var foldsToolbar: Bool { isCompact }

    /// Tallest a bottom dock may be before its content scrolls.
    var dockMaxHeight: CGFloat {
        guard size.height > 0 else { return .infinity }
        return isCompact ? max(150, size.height * 0.5) : max(240, size.height * 0.62)
    }
}

extension EnvironmentValues {
    /// The window is small: panels use their narrow, iPhone-style layouts.
    @Entry var compactLayout = false
    /// Tallest a bottom dock may be (its content scrolls beyond that).
    @Entry var dockMaxHeight: CGFloat = .infinity
}

/// Shows `content` as it is when it fits under the dock's height limit, otherwise in a vertical
/// scroll view of that height. `reserved` is the height the dock needs around the content
/// (grabber, header, padding).
struct DockHeightLimit<Content: View>: View {
    var reserved: CGFloat = 0
    @ViewBuilder var content: Content

    @Environment(\.dockMaxHeight) private var dockMaxHeight

    var body: some View {
        if dockMaxHeight.isFinite {
            HeightCap(maxHeight: max(80, dockMaxHeight - reserved)) {
                ViewThatFits(in: .vertical) {
                    content
                    ScrollView(.vertical) {
                        content
                    }
                    .scrollBounceBehavior(.basedOnSize)
                }
            }
        } else {
            content
        }
    }
}

/// Offers its content `maxHeight` (or less, if that's all the parent offers, e.g. above the
/// keyboard) and takes the content's size: the content's own height when it fits, the limit when it
/// scrolls. (A plain `.frame(maxHeight:)` would stretch to the limit even when the content is shorter.)
private struct HeightCap: Layout {
    let maxHeight: CGFloat

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        guard let child = subviews.first else { return .zero }
        let cap = min(maxHeight, proposal.height ?? .infinity)
        let size = child.sizeThatFits(ProposedViewSize(width: proposal.width, height: cap))
        return CGSize(width: size.width, height: min(size.height, cap))
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        subviews.first?.place(at: bounds.origin, anchor: .topLeading,
                              proposal: ProposedViewSize(width: bounds.width, height: bounds.height))
    }
}

extension View {
    /// Writes this view's height into `heights[key]` whenever it changes (used to know whether the
    /// top-right widgets fit).
    func reportsHeight(_ key: String, to heights: Binding<[String: CGFloat]>) -> some View {
        onGeometryChange(for: CGFloat.self) { $0.size.height } action: { height in
            if abs((heights.wrappedValue[key] ?? -1) - height) > 0.5 {
                heights.wrappedValue[key] = height
            }
        }
    }
}
