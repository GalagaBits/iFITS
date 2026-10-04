//
//  RegionPanel.swift
//  iFITS Start
//
//  The bottom dock in R mode.
//

import SwiftUI

/// R mode: shape tools, plus the selected region's name, color, center, size and angle.
struct RegionPanel: View {
    @Bindable var store: RegionStore
    @Binding var expanded: Bool
    let wcs: WCS
    let hasWCS: Bool
    let glassNamespace: Namespace.ID
    var onImport: () -> Void
    var onExport: () -> Void

    var body: some View {
        CollapsibleDock(expanded: $expanded, glassNamespace: glassNamespace) {
            VStack(alignment: .leading, spacing: 10) {
                header
                Divider()
                if let region = store.selected {
                    RegionEditor(region: region, store: store, wcs: wcs, hasWCS: hasWCS)
                        .id(region.id)          // fresh text boxes for each region
                } else {
                    Text(hint)
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }
        } mini: {
            HStack(spacing: 10) {
                Image(systemName: store.selected?.shape.symbol ?? "square.on.circle")
                    .foregroundStyle(store.selected.map { Color(regionHex: $0.colorHex) } ?? Color.primary)
                Text(store.selected?.name ?? "Regions")
                    .font(.subheadline.weight(.semibold))
                Text("\(store.regions.count) total")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                Image(systemName: "chevron.up")
                    .font(.caption.weight(.bold))
            }
        }
    }

    private var hint: String {
        if let tool = store.tool {
            return "Drag on the image to draw the \(tool.title.lowercased()), or tap for a default size."
        }
        return store.regions.isEmpty
            ? "Pick a shape, then drag on the image to draw a region."
            : "Tap a region to select it, or pick a shape to draw a new one."
    }

    private var header: some View {
        HStack(spacing: 12) {
            Label("Regions", systemImage: "square.on.circle")
                .font(.headline)
                .lineLimit(1)
                .fixedSize()
            toolPicker
            Spacer(minLength: 0)
            Menu {
                Button(action: onImport) {
                    Label("Import Regions (.reg)…", systemImage: "square.and.arrow.down.on.square")
                }
                Button(action: onExport) {
                    Label("Export Regions (.reg)…", systemImage: "square.and.arrow.up.on.square")
                }
                .disabled(store.regions.isEmpty)
            } label: {
                Image(systemName: "ellipsis.circle")
                    .font(.title3)
                    .frame(width: 36, height: 36)
                    .contentShape(Rectangle())
            }
            .accessibilityLabel("Region files")
            DockCollapseButton(expanded: $expanded)
        }
    }

    private var toolPicker: some View {
        HStack(spacing: 2) {
            toolButton(nil, symbol: "cursorarrow", label: "Select and move")
            ForEach(RegionShape.allCases) { shape in
                toolButton(shape, symbol: shape.symbol, label: "Draw \(shape.title.lowercased())")
            }
        }
        .padding(3)
        .background(.thinMaterial, in: Capsule())
    }

    private func toolButton(_ shape: RegionShape?, symbol: String, label: String) -> some View {
        let active = store.tool == shape
        return Button {
            withAnimation(.snappy) { store.tool = shape }
        } label: {
            Image(systemName: symbol)
                .font(.body.weight(.medium))
                .foregroundStyle(active ? Color.white : Color.primary)
                .frame(width: 40, height: 32)
                .background {
                    if active { Capsule().fill(Color.orange) }
                }
                .contentShape(Capsule())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityLabel(label)
        .accessibilityAddTraits(active ? .isSelected : [])
    }
}
