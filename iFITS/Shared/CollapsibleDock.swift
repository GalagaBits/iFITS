//
//  CollapsibleDock.swift
//  iFITS Start
//
//  The collapsible bottom dock used by R, S and C modes.
//

import SwiftUI

enum DockAnimation {
    /// The spring used when a dock panel changes size.
    static func stage(_ reduceMotion: Bool) -> Animation {
        reduceMotion ? .smooth(duration: 0.3) : .bouncy(duration: 0.5, extraBounce: 0.12)
    }
}

/// A bottom-dock panel with two sizes: expanded, and a small pill. Shares the Render
/// Configuration panel's glass, so switching modes morphs one panel into the other.
/// Drag the grabber (or the pill) up / down, tap the pill, or use the chevron to resize.
struct CollapsibleDock<Full: View, Mini: View>: View {
    @Binding var expanded: Bool
    let glassNamespace: Namespace.ID
    private let full: Full
    private let mini: Mini

    @State private var dragY: CGFloat = 0
    @State private var isDragging = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    init(expanded: Binding<Bool>, glassNamespace: Namespace.ID,
         @ViewBuilder full: () -> Full, @ViewBuilder mini: () -> Mini) {
        _expanded = expanded
        self.glassNamespace = glassNamespace
        self.full = full()
        self.mini = mini()
    }

    var body: some View {
        Group {
            if expanded {
                VStack(spacing: 6) {
                    grabber
                    full
                }
                .padding(.horizontal, 20)
                .padding(.top, 4)
                .padding(.bottom, 16)
                .frame(maxWidth: 1000)
                .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 28, style: .continuous))
                .glassEffectID("renderPanel", in: glassNamespace)
            } else {
                mini
                    .padding(.horizontal, 16)
                    .padding(.vertical, 12)
                    .contentShape(Capsule())
                    .glassEffect(.regular.interactive(), in: Capsule())
                    .glassEffectID("renderPanel", in: glassNamespace)
                    .onTapGesture {
                        withAnimation(DockAnimation.stage(reduceMotion)) { expanded = true }
                    }
                    .gesture(stageDrag)
                    .hoverEffect(.lift)
                    .accessibilityElement(children: .combine)
                    .accessibilityAddTraits(.isButton)
            }
        }
        .scaleEffect(x: dragEffect.scaleX, y: dragEffect.scaleY, anchor: .bottom)
        .offset(y: dragEffect.offset)
        .sensoryFeedback(.impact(weight: .light), trigger: expanded)
    }

    private var grabber: some View {
        Capsule()
            .fill(isDragging ? AnyShapeStyle(.primary) : AnyShapeStyle(.secondary))
            .frame(width: isDragging ? 56 : 40, height: 5)
            .animation(.snappy(duration: 0.2), value: isDragging)
            .frame(maxWidth: .infinity, minHeight: 18)
            .contentShape(Rectangle())
            .onTapGesture {
                withAnimation(DockAnimation.stage(reduceMotion)) { expanded = false }
            }
            .gesture(stageDrag)
            .accessibilityLabel("Minimize panel")
    }

    /// Drag up to expand, down to shrink; the panel follows and stretches, then springs into place.
    private var stageDrag: some Gesture {
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
                var target = expanded
                if moved < -40 || predicted < -120 {
                    target = true
                } else if moved > 40 || predicted > 120 {
                    target = false
                }
                withAnimation(DockAnimation.stage(reduceMotion)) {
                    expanded = target
                    dragY = 0
                    isDragging = false
                }
            }
    }

    private func rubberBand(_ distance: CGFloat, limit: CGFloat) -> CGFloat {
        guard distance > 0 else { return 0 }
        return limit * (1 - 1 / (distance * 0.55 / limit + 1))
    }

    private var dragEffect: (offset: CGFloat, scaleX: CGFloat, scaleY: CGFloat) {
        guard !reduceMotion, dragY != 0 else { return (0, 1, 1) }
        if dragY < 0 {
            if !expanded {
                let r = rubberBand(-dragY, limit: 120)
                return (-r * 0.6, 1 + r / 2400, 1 + r / 800)
            }
            let r = rubberBand(-dragY, limit: 70)
            return (-r * 0.35, 1 - r / 2800, 1 + r / 900)
        } else {
            if expanded {
                let r = rubberBand(dragY, limit: 140)
                return (r * 0.7, 1 - r / 2800, 1 - r / 1000)
            }
            let r = rubberBand(dragY, limit: 50)
            return (r * 0.45, 1 + r / 500, 1 - r / 220)
        }
    }
}

/// Chevron that shrinks a dock panel to its pill.
struct DockCollapseButton: View {
    @Binding var expanded: Bool
    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    var body: some View {
        Button {
            withAnimation(DockAnimation.stage(reduceMotion)) { expanded = false }
        } label: {
            Image(systemName: "chevron.down")
                .font(.body.weight(.semibold))
                .frame(width: 36, height: 36)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityLabel("Minimize panel")
    }
}

/// Label for a pop-up menu in a dock panel (same look as the Render Configuration menus).
struct DockMenuLabel: View {
    let text: String

    var body: some View {
        HStack(spacing: 6) {
            Text(text).lineLimit(1)
            Spacer(minLength: 4)
            Image(systemName: "chevron.up.chevron.down")
                .font(.caption2.weight(.semibold))
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 7)
        .frame(minWidth: 160)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
        .contentShape(Rectangle())
    }
}
