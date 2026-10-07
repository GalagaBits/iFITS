//
//  RenderConfigPanel.swift
//  iFITS Start
//
//  The Render Configuration panel (V mode).
//

import SwiftUI

enum PanelStage: Int {
    case mini, compact, full
    var larger: PanelStage { PanelStage(rawValue: min(rawValue + 1, 2)) ?? .full }
    var smaller: PanelStage { PanelStage(rawValue: max(rawValue - 1, 0)) ?? .mini }
}

/// CARTA-style render configuration that docks at the bottom of the screen.
/// Three sizes: full (histogram + all controls), compact (one row), and mini (a small pill).
/// Drag the grabber up/down, or use the buttons, to switch sizes.
struct RenderConfigPanel: View {
    @Binding var settings: RenderSettings
    @Binding var stage: PanelStage
    let clipSelection: ClipSelection
    let stats: ImageStats
    /// Shared with the Annotation bar so the glass can morph between them.
    let glassNamespace: Namespace.ID
    var onSelectPercentile: (Double) -> Void
    var onManualClip: (Double, Double) -> Void

    static let percentilePresets: [Double] = [90, 95, 99, 99.5, 99.9, 99.95, 99.99, 100]

    @State private var showColormaps = false
    @State private var showCustomPercentile = false
    @State private var customPercentileText = ""

    // Drag / morph animation state
    @State private var dragY: CGFloat = 0
    @State private var isDragging = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    /// Small window: the histogram sits above the controls, and the panel scrolls if it's too tall.
    @Environment(\.compactLayout) private var compact

    /// Spring used for every stage change: a modest iOS-style bounce.
    private var stageAnimation: Animation {
        reduceMotion ? .smooth(duration: 0.3) : .bouncy(duration: 0.5, extraBounce: 0.12)
    }

    var body: some View {
        // The GlassEffectContainer lives in ContentView, so the glass morphs between
        // stages here and into the Annotation bar in A mode.
        content
        .scaleEffect(x: dragEffect.scaleX, y: dragEffect.scaleY, anchor: .bottom)
        .offset(y: dragEffect.offset)
        .sensoryFeedback(.impact(weight: .light), trigger: stage)
        .alert("Custom Clip Percentile", isPresented: $showCustomPercentile) {
            TextField("e.g. 99.7", text: $customPercentileText)
                .keyboardType(.decimalPad)
            Button("Apply") { applyCustomPercentile() }
            Button("Cancel", role: .cancel) {}
        } message: {
            Text("Percent of pixels to keep between Clip min and Clip max (0–100).")
        }
    }

    @ViewBuilder
    private var content: some View {
        switch stage {
        case .full: fullPanel
        case .compact: compactPanel
        case .mini: miniPanel
        }
    }

    // MARK: Stages

    private var fullPanel: some View {
        VStack(alignment: .leading, spacing: 10) {
            grabber
            HStack {
                Label("Render Configuration", systemImage: "slider.horizontal.3")
                    .font(.headline)
                    .lineLimit(1)
                Spacer(minLength: 4)
                stageButton("rectangle.compress.vertical", to: .compact, help: "Smaller panel")
                stageButton("chevron.down", to: .mini, help: "Minimize panel")
            }
            .contentShape(Rectangle())
            .gesture(stageDrag)

            // Grabber, header, spacing and padding take about 90 points.
            DockHeightLimit(reserved: 90) {
                // Side by side when there's room; otherwise the histogram above the controls.
                ViewThatFits(in: .horizontal) {
                    HStack(alignment: .top, spacing: 20) {
                        histogram(height: 170)
                            .frame(minWidth: 260, maxWidth: .infinity)
                        controlsGrid
                            .frame(width: 340)
                    }
                    VStack(alignment: .leading, spacing: 14) {
                        histogram(height: compact ? 110 : 150)
                        controlsGrid
                    }
                }
            }
        }
        .padding(.horizontal, compact ? 14 : 20)
        .padding(.top, 4)
        .padding(.bottom, 16)
        .frame(maxWidth: 1000)
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 28, style: .continuous))
        .glassEffectID("renderPanel", in: glassNamespace)
    }

    /// The histogram with its range and hint underneath.
    private func histogram(height: CGFloat) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            ClipHistogramView(stats: stats, settings: settings) { lo, hi in
                onManualClip(lo, hi)
            }
            .frame(height: height)
            HStack {
                Text(NumberField.format(stats.histLo))
                Spacer(minLength: 4)
                if !compact {
                    Text("Pixel value · drag the red lines")
                        .lineLimit(1)
                    Spacer(minLength: 4)
                }
                Text(NumberField.format(stats.histHi))
            }
            .font(.caption2.monospacedDigit())
            .foregroundStyle(.secondary)
            .lineLimit(1)
        }
    }

    private var compactPanel: some View {
        VStack(spacing: 4) {
            grabber
            HStack(alignment: .bottom, spacing: 12) {
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(alignment: .bottom, spacing: 12) {
                        compactItem("Clip min") {
                            NumberField(value: settings.clipMin) { onManualClip($0, settings.clipMax) }
                                .frame(width: 104)
                        }
                        compactItem("Clip max") {
                            NumberField(value: settings.clipMax) { onManualClip(settings.clipMin, $0) }
                                .frame(width: 104)
                        }
                        compactItem("Clip %") { percentileMenu }
                        compactItem("Scaling") { scalingMenu }
                        compactItem("Colormap") { colormapButton }
                        compactItem("Invert") {
                            Toggle("Invert colormap", isOn: $settings.inverted)
                                .labelsHidden()
                        }
                    }
                    .padding(.vertical, 2)
                }
                HStack(spacing: 0) {
                    stageButton("rectangle.expand.vertical", to: .full, help: "Larger panel")
                    stageButton("chevron.down", to: .mini, help: "Minimize panel")
                }
            }
        }
        .padding(.horizontal, compact ? 12 : 18)
        .padding(.top, 2)
        .padding(.bottom, 12)
        .frame(maxWidth: 1000)
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 24, style: .continuous))
        .glassEffectID("renderPanel", in: glassNamespace)
    }

    private var miniPanel: some View {
        HStack(spacing: 10) {
            ColormapSwatch(colormap: settings.colormap, inverted: settings.inverted)
                .frame(width: 44, height: 12)
            Text(settings.scaling.title)
                .font(.subheadline.weight(.semibold))
                .lineLimit(1)
            Text(clipSelection.title)
                .font(.subheadline.monospacedDigit())
                .foregroundStyle(.secondary)
                .lineLimit(1)
            Image(systemName: "chevron.up")
                .font(.caption.weight(.bold))
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
        .contentShape(Capsule())
        .glassEffect(.regular.interactive(), in: Capsule())
        .glassEffectID("renderPanel", in: glassNamespace)
        .onTapGesture {
            withAnimation(stageAnimation) { stage = .compact }
        }
        .gesture(stageDrag)
        .hoverEffect(.lift)
        .accessibilityElement(children: .combine)
        .accessibilityAddTraits(.isButton)
        .accessibilityLabel("Show render configuration")
    }

    // MARK: Pieces

    private var grabber: some View {
        Capsule()
            .fill(isDragging ? AnyShapeStyle(.primary) : AnyShapeStyle(.secondary))
            .frame(width: isDragging ? 56 : 40, height: 5)
            .animation(.snappy(duration: 0.2), value: isDragging)
            .frame(maxWidth: .infinity, minHeight: 18)
            .contentShape(Rectangle())
            .onTapGesture {
                withAnimation(stageAnimation) { stage = stage == .full ? .compact : .full }
            }
            .gesture(stageDrag)
            .accessibilityLabel("Resize panel")
    }

    /// Drag up to grow, down to shrink. The panel follows your finger while dragging,
    /// then springs into the new stage. A fast flick can skip a stage.
    private var stageDrag: some Gesture {
        // Global space, so moving/stretching the panel doesn't feed back into the drag.
        DragGesture(minimumDistance: 6, coordinateSpace: .global)
            .onChanged { value in
                withAnimation(.interactiveSpring(response: 0.18, dampingFraction: 0.86)) {
                    isDragging = true
                    dragY = value.translation.height
                }
            }
            .onEnded { value in
                let moved = value.translation.height
                let predicted = value.predictedEndTranslation.height
                var target = stage
                if moved < -40 || predicted < -120 {
                    target = stage.larger
                    if predicted < -420 { target = target.larger }
                } else if moved > 40 || predicted > 120 {
                    target = stage.smaller
                    if predicted > 420 { target = target.smaller }
                }
                withAnimation(stageAnimation) {
                    stage = target
                    dragY = 0
                    isDragging = false
                }
            }
    }

    /// Diminishing-returns pull, like iOS rubber-banding: approaches `limit`, never passes it.
    private func rubberBand(_ distance: CGFloat, limit: CGFloat) -> CGFloat {
        guard distance > 0 else { return 0 }
        return limit * (1 - 1 / (distance * 0.55 / limit + 1))
    }

    /// How far to move and how much to stretch the panel for the current drag.
    private var dragEffect: (offset: CGFloat, scaleX: CGFloat, scaleY: CGFloat) {
        guard !reduceMotion, dragY != 0 else { return (0, 1, 1) }
        if dragY < 0 {
            let r: CGFloat
            if stage != .full {
                // Heading to a bigger stage: follow the finger and start to grow.
                r = rubberBand(-dragY, limit: 120)
                return (-r * 0.6, 1 + r / 2400, 1 + r / 800)
            }
            // Already full: stretch upward like a rubber band, then bounce back.
            r = rubberBand(-dragY, limit: 70)
            return (-r * 0.35, 1 - r / 2800, 1 + r / 900)
        } else {
            let r: CGFloat
            if stage != .mini {
                // Heading to a smaller stage: follow the finger and start to shrink.
                r = rubberBand(dragY, limit: 140)
                return (r * 0.7, 1 - r / 2800, 1 - r / 1000)
            }
            // Already mini: squash down into itself, then pop back.
            r = rubberBand(dragY, limit: 50)
            return (r * 0.45, 1 + r / 500, 1 - r / 220)
        }
    }

    private func stageButton(_ systemName: String, to target: PanelStage, help: String) -> some View {
        Button {
            withAnimation(stageAnimation) { stage = target }
        } label: {
            Image(systemName: systemName)
                .font(.body.weight(.semibold))
                .frame(width: 36, height: 36)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityLabel(help)
    }

    private var controlsGrid: some View {
        Grid(alignment: .leading, horizontalSpacing: 12, verticalSpacing: 10) {
            GridRow {
                rowLabel("Clip Percentile")
                percentileMenu
            }
            GridRow {
                rowLabel("Clip min")
                NumberField(value: settings.clipMin) { onManualClip($0, settings.clipMax) }
            }
            GridRow {
                rowLabel("Clip max")
                NumberField(value: settings.clipMax) { onManualClip(settings.clipMin, $0) }
            }
            GridRow {
                rowLabel("Scaling")
                scalingMenu
            }
            if settings.scaling.usesAlpha {
                GridRow {
                    rowLabel("Alpha (α)")
                    alphaSlider
                }
            }
            if settings.scaling == .gamma {
                GridRow {
                    rowLabel("Gamma (γ)")
                    gammaSlider
                }
            }
            GridRow {
                rowLabel("Colormap")
                colormapButton
            }
            GridRow {
                rowLabel("Invert colormap")
                Toggle("Invert colormap", isOn: $settings.inverted)
                    .labelsHidden()
            }
        }
    }

    private func rowLabel(_ text: String) -> some View {
        Text(text)
            .font(compact ? .caption : .subheadline)
            .lineLimit(1)
            .foregroundStyle(.secondary)
            .gridColumnAlignment(.trailing)
    }

    private func compactItem<Content: View>(_ title: String, @ViewBuilder content: () -> Content) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.caption2)
                .foregroundStyle(.secondary)
            content()
        }
    }

    private func menuLabel(_ text: String) -> some View {
        HStack(spacing: 6) {
            Text(text).lineLimit(1)
            Spacer(minLength: 4)
            Image(systemName: "chevron.up.chevron.down")
                .font(.caption2.weight(.semibold))
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 7)
        .frame(minWidth: compact ? 90 : 110)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
        .contentShape(Rectangle())
    }

    /// Current percentile if it isn't one of the presets.
    private var customPercentile: Double? {
        if case .percentile(let p) = clipSelection, !Self.percentilePresets.contains(p) { return p }
        return nil
    }

    private var percentileMenu: some View {
        Menu {
            ForEach(Self.percentilePresets, id: \.self) { p in
                Button {
                    onSelectPercentile(p)
                } label: {
                    if clipSelection == .percentile(p) {
                        Label(ClipSelection.percentText(p), systemImage: "checkmark")
                    } else {
                        Text(ClipSelection.percentText(p))
                    }
                }
            }
            Divider()
            Button {
                if case .percentile(let p) = clipSelection {
                    customPercentileText = p.formatted(.number.precision(.fractionLength(0...4)).grouping(.never))
                } else {
                    customPercentileText = ""
                }
                showCustomPercentile = true
            } label: {
                if let p = customPercentile {
                    Label("Custom (\(ClipSelection.percentText(p)))…", systemImage: "checkmark")
                } else {
                    Text("Custom…")
                }
            }
        } label: {
            menuLabel(clipSelection.title)
        }
        .menuOrder(.fixed)
    }

    private var scalingMenu: some View {
        Menu {
            ForEach(ScalingType.allCases) { s in
                Button {
                    settings.scaling = s
                } label: {
                    // Two Texts in a menu item = title + subtitle (the formula).
                    if s == settings.scaling {
                        Label(s.title, systemImage: "checkmark")
                    } else {
                        Text(s.title)
                    }
                    Text(s.formula)
                }
            }
        } label: {
            menuLabel(settings.scaling.title)
        }
    }

    private var alphaSlider: some View {
        HStack {
            Slider(value: Binding(get: { log10(settings.alpha) },
                                  set: { settings.alpha = pow(10, $0) }),
                   in: 0.3...5)
            Text(settings.alpha, format: .number.precision(.significantDigits(3)))
                .font(.caption.monospacedDigit())
                .frame(width: 52, alignment: .trailing)
        }
    }

    private var gammaSlider: some View {
        HStack {
            Slider(value: $settings.gamma, in: 0.1...5)
            Text(String(format: "%.2f", settings.gamma))
                .font(.caption.monospacedDigit())
                .frame(width: 52, alignment: .trailing)
        }
    }

    private var colormapButton: some View {
        Button {
            showColormaps = true
        } label: {
            HStack(spacing: 8) {
                ColormapSwatch(colormap: settings.colormap, inverted: settings.inverted)
                    .frame(width: compact ? 44 : 90, height: 14)
                Text(settings.colormap.title)
                    .lineLimit(1)
                Image(systemName: "chevron.up.chevron.down")
                    .font(.caption2.weight(.semibold))
            }
            .padding(.horizontal, 10)
            .padding(.vertical, 7)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .popover(isPresented: $showColormaps) {
            ScrollView {
                VStack(alignment: .leading, spacing: 2) {
                    ForEach(Colormap.allCases) { cm in
                        Button {
                            settings.colormap = cm
                        } label: {
                            HStack(spacing: 12) {
                                ColormapSwatch(colormap: cm, inverted: settings.inverted)
                                    .frame(width: 150, height: 18)
                                Text(cm.title)
                                Spacer()
                                if cm == settings.colormap {
                                    Image(systemName: "checkmark").foregroundStyle(.tint)
                                }
                            }
                            .padding(.horizontal, 12)
                            .padding(.vertical, 8)
                            .background(cm == settings.colormap ? Color.accentColor.opacity(0.12) : .clear,
                                        in: RoundedRectangle(cornerRadius: 8))
                            .contentShape(Rectangle())
                        }
                        .buttonStyle(.plain)
                    }
                    Divider().padding(.vertical, 4)
                    Toggle("Invert colormap", isOn: $settings.inverted)
                        .padding(.horizontal, 12)
                        .padding(.vertical, 6)
                }
                .padding(8)
            }
            .frame(width: 320, height: 470)
            .presentationCompactAdaptation(.popover)
        }
    }

    private func applyCustomPercentile() {
        let cleaned = customPercentileText
            .replacingOccurrences(of: "%", with: "")
            .replacingOccurrences(of: ",", with: ".")
            .trimmingCharacters(in: .whitespaces)
        guard let p = Double(cleaned), p > 0, p <= 100 else { return }
        onSelectPercentile(p)
    }
}
