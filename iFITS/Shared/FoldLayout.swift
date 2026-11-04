//
//  FoldLayout.swift
//  iFITS
//
//  iPhone Duo: where the fold (crease) is while the phone is partly open, so the UI can stay off it.
//  - Book pose (vertical crease): the bottom docks stay to the left of it.
//  - Laptop pose (horizontal crease): the colorbar stays above it and the bottom docks below it.
//

import SwiftUI

/// Keeps `fold` up to date with the crease's frame in window (global) coordinates, or nil when
/// there's no active fold: the phone is flat, closed, or it isn't an iPhone Duo. The reserved
/// region API needs iOS 27.1, so older systems always get nil.
struct FoldReader: ViewModifier {
    @Binding var fold: CGRect?

    func body(content: Content) -> some View {
        if #available(iOS 27.1, *) {
            content.modifier(FoldRegionReader(fold: $fold))
        } else {
            content
        }
    }
}

@available(iOS 27.1, *)
private struct FoldRegionReader: ViewModifier {
    @Binding var fold: CGRect?

    func body(content: Content) -> some View {
        content.onGeometryChange(for: CGRect?.self) { proxy in
            // Only active regions: the fold is active only while the phone is partly open.
            guard let region = proxy.reservedRegions(kind: .division).first else { return nil }
            // The region's frame is in this view's own coordinates; the rest of the layout uses
            // window coordinates.
            let origin = proxy.frame(in: .global).origin
            return region.frame.offsetBy(dx: origin.x, dy: origin.y)
        } action: { newFold in
            withAnimation(.smooth) { fold = newFold }
        }
    }
}
