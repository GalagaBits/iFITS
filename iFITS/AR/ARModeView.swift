//
//  ARModeView.swift
//  iFITS Start
//
//  The AR screen: the cube in 3-D (or in the room through the camera), with a back arrow (top
//  left), camera and options buttons (top right), and at the bottom left the colorbar (same place
//  and look as the main view's) and the 3-D grid button (like the V A R S buttons).
//

import SwiftUI

struct ARModeView: View {
    @State private var model: ARModel
    @State private var colorbarExpanded = true
    var onClose: () -> Void

    init(source: ARSource, onClose: @escaping () -> Void) {
        _model = State(initialValue: ARModel(source: source))
        self.onClose = onClose
    }

    var body: some View {
        ZStack {
            Color.black.ignoresSafeArea()

            if let volume = model.volume {
                ARVolumeView(volume: volume,
                             axes: model.axes,
                             settings: model.settings,
                             onLabels: { labels in
                                 if model.labels != labels { model.labels = labels }
                             },
                             onStatus: { model.status = $0 },
                             onRoomFailed: { model.inRoom = false },
                             onUnavailable: {
                                 model.failure = "3-D drawing isn't available. Check that ARShaders.metal is part of the iFITS target."
                             })
                    .ignoresSafeArea()
                ARLabelLayer(model: model)
            } else if model.failure == nil {
                VStack(spacing: 12) {
                    ProgressView(value: model.progress)
                        .frame(width: 260)
                    Text("Reading every channel of the cube…")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
            }

            if let failure = model.failure {
                Text(failure)
                    .font(.callout)
                    .multilineTextAlignment(.center)
                    .padding(16)
                    .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 16, style: .continuous))
                    .padding(40)
            }

            controls
        }
        .preferredColorScheme(.dark)
        .task { await model.load() }
    }

    // MARK: Controls

    private var controls: some View {
        VStack(spacing: 0) {
            // Top: back (left); title (middle); camera and options (right).
            HStack(alignment: .top, spacing: 14) {
                circleButton(systemImage: "chevron.left", label: "Back to the image", action: onClose)
                Spacer()
                VStack(spacing: 2) {
                    Text(model.source.fileName)
                        .font(.headline)
                        .lineLimit(1)
                    Text(model.inRoom ? "In the room" : "3-D cube")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                .padding(.horizontal, 16)
                .padding(.vertical, 8)
                .glassEffect(.regular, in: Capsule())
                Spacer()
                circleButton(systemImage: "arkit", label: model.inRoom ? "Leave the room" : "Place the cube in the room",
                             highlighted: model.inRoom) {
                    model.status = nil
                    model.inRoom.toggle()
                }
                .disabled(model.volume == nil)
                optionsMenu
            }
            .padding(.horizontal, 24)
            .padding(.top, 12)

            Spacer()

            // Bottom left: the colorbar (same place as in the main view), then the 3-D grid button;
            // hints in the middle.
            HStack(alignment: .bottom, spacing: 16) {
                if model.volume != nil {
                    let r = model.range
                    ARColorbar(colormap: model.colormap, inverted: model.inverted, lo: r.lo, hi: r.hi,
                               unit: model.unitText,
                               expanded: $colorbarExpanded,
                               onSelect: { model.colormap = $0 },
                               onToggleInverted: { model.inverted.toggle() })
                }
                gridButton
                Spacer()
                hint
                Spacer()
                Color.clear.frame(width: 70, height: 1)
            }
            .padding(.leading, 40)
            .padding(.trailing, 24)
            .padding(.bottom, 16)
        }
    }

    /// Same look as the V A R S buttons: a glass circle, orange while on.
    private var gridButton: some View {
        Button {
            withAnimation(.snappy) { model.showGrid.toggle() }
        } label: {
            ZStack {
                if model.showGrid {
                    Circle().fill(.orange.opacity(0.95))
                }
                Circle()
                    .glassEffect(.regular, in: Circle())
                Image(systemName: "cube.transparent")
                    .font(.system(size: 30, weight: .regular))
                    .foregroundStyle(.primary)
            }
            .frame(width: 70, height: 70)
            .contentShape(Circle())
            .hoverEffect(.lift)
        }
        .buttonStyle(.plain)
        .disabled(model.volume == nil)
        .accessibilityLabel(model.showGrid ? "Hide the 3-D grid" : "Show the 3-D grid")
    }

    private var optionsMenu: some View {
        Menu {
            Button {
                model.resetToken += 1
            } label: {
                Label("Reset View", systemImage: "arrow.counterclockwise")
            }
            Picker(selection: $model.opacity) {
                ForEach(AROpacity.allCases) { level in
                    Text(level.title).tag(level)
                }
            } label: {
                Label("Opacity", systemImage: "circle.lefthalf.filled")
            }
            .pickerStyle(.menu)
            Picker(selection: $model.percentile) {
                if model.source.percentile == nil {
                    Text("Main view's range").tag(Double?.none)
                }
                ForEach(model.percentileChoices, id: \.self) { p in
                    Text(ClipSelection.percentText(p)).tag(Double?.some(p))
                }
            } label: {
                Label("Value Range", systemImage: "slider.horizontal.below.rectangle")
            }
            .pickerStyle(.menu)
            Picker(selection: $model.depthStretch) {
                Text("Half").tag(Float(0.5))
                Text("Same as the image").tag(Float(1))
                Text("Double").tag(Float(2))
            } label: {
                Label("Depth", systemImage: "arrow.up.left.and.arrow.down.right")
            }
            .pickerStyle(.menu)
        } label: {
            Image(systemName: "ellipsis")
                .font(.title2.weight(.semibold))
                .frame(width: 56, height: 56)
                .contentShape(Circle())
                .glassEffect(.regular.interactive(), in: Circle())
        }
        .menuOrder(.fixed)
        .disabled(model.volume == nil)
        .accessibilityLabel("Options")
    }

    private func circleButton(systemImage: String, label: String, highlighted: Bool = false,
                              action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: systemImage)
                .font(.title2.weight(.semibold))
                .foregroundStyle(.primary)
                .frame(width: 56, height: 56)
                .background {
                    if highlighted { Circle().fill(.orange.opacity(0.95)) }
                }
                .contentShape(Circle())
                .glassEffect(.regular.interactive(), in: Circle())
        }
        .buttonStyle(.plain)
        .accessibilityLabel(label)
    }

    private var defaultHint: String? {
        guard model.volume != nil else { return nil }
        return model.inRoom
            ? "Tap a table or the floor to move the cube there. Pinch to resize, drag to turn it."
            : "Drag to rotate · Pinch to zoom · Twist to roll · Double-tap to reset"
    }

    /// What to do next, or a message from the camera.
    @ViewBuilder
    private var hint: some View {
        let text: String? = model.status ?? defaultHint
        if let text {
            Text(text)
                .font(.callout)
                .multilineTextAlignment(.center)
                .padding(.horizontal, 16)
                .padding(.vertical, 10)
                .glassEffect(.regular, in: Capsule())
                .frame(maxWidth: 620)
                .allowsHitTesting(false)
        }
    }
}

/// The grid's tick labels and axis titles, placed where the renderer projected them. Its own view,
/// so moving labels (every frame in camera mode) only redraws this layer.
private struct ARLabelLayer: View {
    let model: ARModel

    var body: some View {
        ZStack {
            ForEach(model.labels) { label in
                Text(label.text)
                    .font(label.isTitle ? .caption.weight(.semibold) : .caption2.monospacedDigit())
                    .foregroundStyle(.white)
                    .shadow(color: .black, radius: 2)
                    .fixedSize()
                    .position(label.point)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .ignoresSafeArea()
        .allowsHitTesting(false)
    }
}
