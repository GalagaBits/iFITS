//
//  RegionEditor.swift
//  iFITS Start
//
//  Editing the selected region's name, color, center, size and angle.
//

import SwiftUI

/// The selected region's settings. Values apply when you press Return or leave a box.
struct RegionEditor: View {
    let region: FITSRegion
    let store: RegionStore
    let wcs: WCS
    let hasWCS: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(spacing: 8) {
                Image(systemName: region.shape.symbol)
                    .foregroundStyle(Color(regionHex: region.colorHex))
                Text(region.name)
                    .font(.title3.weight(.semibold))
                    .lineLimit(1)
                Text(region.shape.title)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                Spacer()
                Button(role: .destructive) {
                    withAnimation(.snappy) { store.delete(region.id) }
                } label: {
                    Image(systemName: "trash")
                        .frame(width: 36, height: 32)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .foregroundStyle(.red)
                .hoverEffect(.highlight)
                .accessibilityLabel("Delete \(region.name)")
            }

            ViewThatFits(in: .horizontal) {
                wideLayout
                narrowLayout
            }
        }
    }

    // MARK: Layouts

    private var wideLayout: some View {
        Grid(alignment: .leading, horizontalSpacing: 12, verticalSpacing: 8) {
            GridRow {
                label("Name")
                nameField.frame(width: 214)
                label("Color")
                colorPicker
            }
            GridRow {
                label("Center (px)")
                centerPixelFields
                label("Center (WCS)")
                centerWorldFields(stacked: false)
            }
            GridRow {
                label(sizeLabel)
                sizeColumn
                label("P.A. (deg)")
                angleField
            }
        }
        .fixedSize(horizontal: true, vertical: false)
    }

    private var narrowLayout: some View {
        Grid(alignment: .leading, horizontalSpacing: 10, verticalSpacing: 8) {
            GridRow {
                label("Name")
                nameField.frame(maxWidth: 214)
            }
            GridRow {
                label("Color")
                ScrollView(.horizontal, showsIndicators: false) { colorPicker }
            }
            GridRow {
                label("Center (px)")
                centerPixelFields
            }
            GridRow {
                label("Center (WCS)")
                centerWorldFields(stacked: true)
            }
            GridRow {
                label(sizeLabel)
                sizeColumn
            }
            GridRow {
                label("P.A. (deg)")
                angleField
            }
        }
    }

    // MARK: Fields

    private func edit(_ change: (inout FITSRegion) -> Void) {
        store.update(region.id, change)
    }

    private func label(_ text: String) -> some View {
        Text(text)
            .font(.subheadline)
            .foregroundStyle(.secondary)
            .lineLimit(1)
            .gridColumnAlignment(.trailing)
    }

    private var nameField: some View {
        InlineTextField(text: region.name) { text in
            let name = text.trimmingCharacters(in: .whitespacesAndNewlines)
            if !name.isEmpty { edit { $0.name = name } }
        }
    }

    private var colorPicker: some View {
        HStack(spacing: 4) {
            ForEach(RegionColor.palette) { color in
                let isCurrent = color.hex == region.colorHex.uppercased()
                Button {
                    edit { $0.colorHex = color.hex }
                    store.newRegionColor = color.hex
                } label: {
                    Circle()
                        .fill(Color(regionHex: color.hex))
                        .frame(width: 22, height: 22)
                        .overlay(Circle().stroke(Color.black.opacity(0.25), lineWidth: 0.5))
                        .overlay(Circle().stroke(Color.primary, lineWidth: isCurrent ? 2 : 0).padding(-3))
                        .frame(width: 30, height: 30)
                        .contentShape(Circle())
                }
                .buttonStyle(.plain)
                .hoverEffect(.lift)
                .accessibilityLabel(color.name)
                .accessibilityAddTraits(isCurrent ? .isSelected : [])
            }
        }
    }

    private var centerPixelFields: some View {
        HStack(spacing: 6) {
            NumberField(value: Double(region.center.x)) { v in edit { $0.center.x = v } }
                .frame(width: 104)
            NumberField(value: Double(region.center.y)) { v in edit { $0.center.y = v } }
                .frame(width: 104)
        }
    }

    @ViewBuilder
    private func centerWorldFields(stacked: Bool) -> some View {
        if hasWCS {
            let world = wcs.pixelToWorld(Double(region.center.x), Double(region.center.y))
            let ra = InlineTextField(text: DS9Regions.raText(world.0), monospaced: true, alignment: .trailing) { text in
                if let v = DS9Regions.parseSky(text, isRA: true) { moveCenter(ra: v, dec: world.1) }
            }
            .frame(width: 150)
            let dec = InlineTextField(text: DS9Regions.decText(world.1), monospaced: true, alignment: .trailing) { text in
                if let v = DS9Regions.parseSky(text, isRA: false) { moveCenter(ra: world.0, dec: v) }
            }
            .frame(width: 150)
            if stacked {
                VStack(alignment: .leading, spacing: 6) { ra; dec }
            } else {
                HStack(spacing: 6) { ra; dec }
            }
        } else {
            Text("No celestial WCS")
                .font(.subheadline)
                .foregroundStyle(.secondary)
        }
    }

    private func moveCenter(ra: Double, dec: Double) {
        guard let p = wcs.worldToPixel(ra, dec) else { return }
        edit { $0.center = CGPoint(x: p.0, y: p.1) }
    }

    private var sizeLabel: String {
        switch region.shape {
        case .ellipse: "Semi-axes (px)"
        case .rectangle: "Size (px)"
        case .line: "Length (px)"
        case .point: "Size (px)"
        }
    }

    private var sizeColumn: some View {
        VStack(alignment: .leading, spacing: 2) {
            sizeFields
            if let sky = skySize {
                Text(sky)
                    .font(.caption.monospacedDigit())
                    .foregroundStyle(.secondary)
            }
        }
    }

    @ViewBuilder
    private var sizeFields: some View {
        switch region.shape {
        case .ellipse:
            HStack(spacing: 6) {
                NumberField(value: Double(region.size.width / 2)) { v in
                    if v > 0 { edit { $0.size.width = v * 2 } }
                }
                .frame(width: 104)
                NumberField(value: Double(region.size.height / 2)) { v in
                    if v > 0 { edit { $0.size.height = v * 2 } }
                }
                .frame(width: 104)
            }
        case .rectangle:
            HStack(spacing: 6) {
                NumberField(value: Double(region.size.width)) { v in
                    if v > 0 { edit { $0.size.width = v } }
                }
                .frame(width: 104)
                NumberField(value: Double(region.size.height)) { v in
                    if v > 0 { edit { $0.size.height = v } }
                }
                .frame(width: 104)
            }
        case .line:
            NumberField(value: Double(region.size.width)) { v in
                if v > 0 { edit { $0.size.width = v } }
            }
            .frame(width: 104)
        case .point:
            Text("—").foregroundStyle(.secondary)
        }
    }

    /// The size on the sky, e.g. 12.34″ × 5.60″.
    private var skySize: String? {
        guard hasWCS else { return nil }
        let s = wcs.pixelScaleArcsec
        guard s > 0 else { return nil }
        func sky(_ pixels: CGFloat) -> String { DS9Regions.angularSize(arcsec: Double(pixels) * s) }
        switch region.shape {
        case .ellipse: return "\(sky(region.size.width / 2)) × \(sky(region.size.height / 2))"
        case .rectangle: return "\(sky(region.size.width)) × \(sky(region.size.height))"
        case .line: return sky(region.size.width)
        case .point: return nil
        }
    }

    @ViewBuilder
    private var angleField: some View {
        if region.shape == .point {
            Text("—").foregroundStyle(.secondary)
        } else {
            NumberField(value: region.angle) { v in edit { $0.angle = FITSRegion.normalized(v) } }
                .frame(width: 104)
        }
    }
}
