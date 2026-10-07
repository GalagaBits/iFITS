//
//  ContentView+Window.swift
//  iFITS Start
//
//  The window as a whole: its size (small windows get the compact, iPhone-style layout; see
//  WindowLayout), FITS files opened from the Files app, and FITS files dropped on the window.
//

import SwiftUI

extension ContentView {
    /// Adds the window-wide pieces to the window's layers.
    func withWindowServices(_ content: some View) -> some View {
        content
            // Small windows: every panel picks its narrow layout, and the docks stay short enough.
            .environment(\.compactLayout, windowLayout.isCompact)
            .environment(\.dockMaxHeight, windowLayout.dockMaxHeight)
            .background { layoutReader }
            // The menu bar acts on whichever iFITS window is in front.
            .background {
                KeyWindowObserver { commandCenter.context = commandContext }
                    .frame(width: 0, height: 0)
            }
            // FITS files dropped anywhere in the window (from Files, Mail, …).
            .background {
                FileDropTarget(isEnabled: acceptsDroppedFiles,
                               onTargeted: { targeted in
                                   withAnimation(.snappy(duration: 0.2)) { isDropTargeted = targeted }
                               },
                               onDrop: { url in prepareToOpen(url: url) })
                    .frame(width: 0, height: 0)
            }
            // A FITS file opened from the Files app ("Open With", or tapping it) or the share sheet.
            .onOpenURL { url in
                guard url.isFileURL else { return }
                prepareToOpen(url: url)
            }
            .onChange(of: windowLayout.isCompact) { _, compact in
                compactLayoutChanged(compact)
            }
            // The one-line Pixel Info went away (new file, A mode): its popover mustn't come back
            // by itself later.
            .onChange(of: inspectedPixel == nil || selectedMode == "A") { _, gone in
                if gone { showPixelInfoPopover = false }
            }
    }

    /// Drops are taken only when nothing covers the window (AR view, sheets, pickers, alerts).
    var acceptsDroppedFiles: Bool {
        arSource == nil && hduPicker == nil && !showSettings && exportRequest == nil
            && headerSheetDocument == nil && !showSpectraSheet && !showPicker && pendingOpen == nil
    }

    /// Entering or leaving the compact layout.
    func compactLayoutChanged(_ compact: Bool) {
        showPixelInfoPopover = false
        // The full Render Configuration is wide: use its one-row size in a small window, and go
        // back to full size afterwards.
        if compact {
            if panelStage == .full {
                renderPanelWasFull = true
                panelStage = .compact
            }
        } else if renderPanelWasFull {
            renderPanelWasFull = false
            if panelStage == .compact { panelStage = .full }
        }
    }

    /// Keeps `windowLayout` and `windowFrame` up to date. It ignores the keyboard, so typing in a box
    /// doesn't switch the window to the compact layout.
    var layoutReader: some View {
        Color.clear
            .ignoresSafeArea(.keyboard)
            .onGeometryChange(for: CGRect.self) { $0.frame(in: .global) } action: { frame in
                windowFrame = frame
                let new = WindowLayout(size: frame.size)
                guard new != windowLayout else { return }
                // The first measurement (at launch) just sets the layout, without animating.
                if windowLayout.size != .zero, new.isCompact != windowLayout.isCompact || new.foldsToolbar != windowLayout.foldsToolbar {
                    withAnimation(.snappy) { windowLayout = new }
                } else {
                    windowLayout = new
                }
            }
            .allowsHitTesting(false)
    }

    /// Outline and label while a FITS file is dragged over the window.
    @ViewBuilder
    var dropHighlight: some View {
        if isDropTargeted {
            ZStack {
                RoundedRectangle(cornerRadius: 24, style: .continuous)
                    .strokeBorder(Color.orange, lineWidth: 4)
                    .padding(6)
                Label("Drop to Open", systemImage: "doc.badge.plus")
                    .font(.headline)
                    .padding(.horizontal, 18)
                    .padding(.vertical, 12)
                    .glassEffect(.regular, in: Capsule())
            }
            .ignoresSafeArea()
            .allowsHitTesting(false)
            .transition(.opacity)
        }
    }

}
