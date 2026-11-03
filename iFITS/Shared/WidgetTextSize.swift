//
//  WidgetTextSize.swift
//  iFITS
//
//  A text size limit for iFITS's small floating widgets.
//

import SwiftUI

extension View {
    /// Caps Dynamic Type for the small floating widgets: Pixel Info, the statistics and spectrum
    /// boxes, the bottom docks (V A R S C Z and their collapsed pills), the colorbar, the small
    /// animator, the V A R S buttons, and the popovers and menus they open.
    ///
    /// Text still grows up to the largest standard size (xxxLarge), but not into the accessibility
    /// sizes, which don't fit in these boxes over the image. Sheets, windows and the toolbar keep
    /// the full range.
    func widgetTextSizeLimit() -> some View {
        dynamicTypeSize(...DynamicTypeSize.xxxLarge)
    }
}
