//
//  ARColorbar.swift
//  iFITS Start
//
//  The AR view's colorbar (bottom left, same place and look as the main view's): colour and
//  transparency for each value (low values clear, high values solid), with tick values in the
//  image's units. Tap it to choose another colormap (AR view only).
//

import SwiftUI

struct ARColorbar: View {
    let colormap: Colormap
    let inverted: Bool
    let lo: Double
    let hi: Double
    let unit: String
    @Binding var expanded: Bool
    var onSelect: (Colormap) -> Void
    var onToggleInverted: () -> Void

    var body: some View {
        VerticalColorbar(colormap: colormap,
                         inverted: inverted,
                         lo: lo,
                         hi: hi,
                         unit: unit,
                         showsOpacity: true,
                         barHeight: 300,
                         expanded: $expanded,
                         onSelect: onSelect,
                         onToggleInverted: onToggleInverted)
    }
}
