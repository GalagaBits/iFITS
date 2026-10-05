//
//  ImageColorbar.swift
//  iFITS Start
//
//  The main view's colorbar (bottom left, under the V A R S buttons). It follows the Render
//  Configuration: colormap, inversion, scaling and clip range. Tapping it changes the colormap,
//  just like the V panel. Same look as the AR view's colorbar (Shared/VerticalColorbar).
//

import SwiftUI

struct ImageColorbar: View {
    @Binding var settings: RenderSettings
    let unit: String
    /// Height of the bar (it shrinks to fit above the bottom dock).
    var barHeight: CGFloat = 300
    /// No room for the full colorbar: just the pill.
    var pillOnly = false
    @Binding var expanded: Bool

    var body: some View {
        let s = settings
        VerticalColorbar(colormap: s.colormap,
                         inverted: s.inverted,
                         lo: s.clipMin,
                         hi: s.clipMax,
                         unit: unit,
                         // With a non-linear scaling, a value's colour sits where the scaling puts it.
                         valuePosition: { x in s.scaling.apply(x, alpha: s.alpha, gamma: s.gamma) },
                         barHeight: barHeight,
                         expanded: $expanded,
                         pillOnly: pillOnly,
                         onSelect: { settings.colormap = $0 },
                         onToggleInverted: { settings.inverted.toggle() })
    }
}
