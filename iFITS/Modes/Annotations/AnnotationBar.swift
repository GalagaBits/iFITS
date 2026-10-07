//
//  AnnotationBar.swift
//  iFITS Start
//
//  The glass tool bar shown in A mode.
//

import SwiftUI

/// Small glass bar shown in A mode. The Render Configuration panel morphs into it.
struct AnnotationBar: View {
    @Bindable var model: AnnotationModel
    let glassNamespace: Namespace.ID

    @State private var confirmClear = false
    /// Small window: just the buttons.
    @Environment(\.compactLayout) private var compact

    var body: some View {
        HStack(spacing: 2) {
            if !compact {
                Label("Annotate", systemImage: "pencil.tip.crop.circle")
                    .font(.headline)
                    .padding(.horizontal, 8)

                Divider().frame(height: 24).padding(.horizontal, 4)
            }

            barButton("arrow.uturn.backward", label: "Undo", disabled: !model.canUndo) {
                model.undoLast()
            }
            barButton("arrow.uturn.forward", label: "Redo", disabled: !model.canRedo) {
                model.redoLast()
            }
            barButton(model.fingerDrawing ? "hand.draw.fill" : "hand.draw",
                      label: model.fingerDrawing ? "Finger drawing on" : "Finger drawing off",
                      active: model.fingerDrawing) {
                model.fingerDrawing.toggle()
            }
            barButton(model.isVisible ? "eye" : "eye.slash",
                      label: model.isVisible ? "Hide annotations" : "Show annotations",
                      active: !model.isVisible) {
                model.isVisible.toggle()
            }
            barButton("paintpalette",
                      label: model.toolsVisible ? "Hide tools" : "Show tools",
                      active: model.toolsVisible) {
                model.toolsVisible.toggle()
            }
            barButton("trash", label: "Clear all", disabled: !model.hasStrokes) {
                confirmClear = true
            }
            .confirmationDialog("Clear all annotations?", isPresented: $confirmClear) {
                Button("Clear All", role: .destructive) { model.clear() }
            } message: {
                Text("You can undo this.")
            }
        }
        .padding(.horizontal, compact ? 6 : 10)
        .padding(.vertical, 8)
        .glassEffect(.regular, in: Capsule())
        .glassEffectID("renderPanel", in: glassNamespace)
    }

    private func barButton(_ systemName: String, label: String, active: Bool = false,
                           disabled: Bool = false, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: systemName)
                .font(.title3)
                .foregroundStyle(active ? Color.orange : Color.primary)
                .contentTransition(.symbolEffect(.replace))
                .frame(width: compact ? 38 : 42, height: 38)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .disabled(disabled)
        .opacity(disabled ? 0.35 : 1)
        .hoverEffect(.highlight)
        .accessibilityLabel(label)
    }
}
