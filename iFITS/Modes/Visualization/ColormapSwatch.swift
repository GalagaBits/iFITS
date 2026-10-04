//
//  ColormapSwatch.swift
//  iFITS Start
//
//  Small colormap preview.
//

import SwiftUI

struct ColormapSwatch: View {
    let colormap: Colormap
    let inverted: Bool

    var body: some View {
        Group {
            if let image = colormap.swatchImage(inverted: inverted) {
                Image(uiImage: image)
                    .resizable()
                    .interpolation(colormap == .tab10 ? .none : .medium)
            } else {
                Color.gray
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 3, style: .continuous))
        .overlay(RoundedRectangle(cornerRadius: 3, style: .continuous).stroke(.white.opacity(0.3), lineWidth: 0.5))
    }
}
