//
//  ContentView+Undo.swift
//  iFITS Start
//
//  Undo / redo for regions, and the Undo button. Annotations record their own steps
//  (AnnotationModel); both go into the window's single history (Shared/EditHistory).
//

import SwiftUI

extension ContentView {
    /// Hooks the history up to the window's undo manager (so ⌘Z, the Edit menu and the
    /// three-finger swipes use it), and annotations to the history.
    func connectUndo(_ manager: UndoManager?) {
        edits.attach(manager)
        annotations.history = edits
        annotations.undo.target = edits.manager
    }

    /// Records the regions' latest change as one undo step. Called whenever the regions change;
    /// while a region is being dragged it waits, so a whole drag is one step.
    func recordRegionChange() {
        guard regionDrag == nil else { return }
        let old = regionStore.recordedRegions
        let new = regionStore.regions
        guard old != new else { return }
        regionStore.recordedRegions = new
        let (name, key) = Self.describeRegionChange(from: old, to: new)
        let store = regionStore
        edits.record(name, coalesce: key,
                     undo: { store.restore(old) },
                     redo: { store.restore(new) })
    }

    /// A name for the step ("Move Region", …), and a key for merging quick repeats (typing a name).
    static func describeRegionChange(from old: [FITSRegion], to new: [FITSRegion]) -> (String, String?) {
        if new.count > old.count { return (new.count - old.count == 1 ? "Add Region" : "Add Regions", nil) }
        if new.count < old.count { return (old.count - new.count == 1 ? "Delete Region" : "Delete Regions", nil) }
        let before = Dictionary(old.map { ($0.id, $0) }, uniquingKeysWith: { a, _ in a })
        let changed = new.filter { before[$0.id] != $0 }
        guard changed.count == 1, let r = changed.first, let o = before[r.id] else { return ("Edit Regions", nil) }
        var renamed = o; renamed.name = r.name
        if renamed == r { return ("Rename Region", "rename-\(r.id)") }
        var recolored = o; recolored.colorHex = r.colorHex
        if recolored == r { return ("Change Region Color", nil) }
        var moved = o; moved.center = r.center
        if moved == r { return ("Move Region", nil) }
        var rotated = o; rotated.angle = r.angle
        if rotated == r { return ("Rotate Region", nil) }
        return ("Edit Region", nil)
    }

    /// A new file: nothing to undo, and nothing unsaved.
    func resetHistory() {
        regionStore.recordedRegions = regionStore.regions
        edits.clear()
        savedEditCount = edits.editCount
    }

    /// Keeps undo and autosave going: connects the window's undo manager, records region changes
    /// (a whole drag as one step), and saves a couple of seconds after the last edit, when the app
    /// goes to the background, and when autosave is turned back on.
    func withUndoAndAutosave(_ content: some View) -> some View {
        content
            .onAppear { connectUndo(undoManager) }
            .onChange(of: undoManager.map { ObjectIdentifier($0) }) { _, _ in connectUndo(undoManager) }
            .onChange(of: regionStore.regions) { _, _ in recordRegionChange() }
            .onChange(of: regionDrag == nil) { _, idle in
                if idle { recordRegionChange() }
            }
            .task(id: edits.editCount) {
                guard autosaveEnabled, hasUnsavedEdits else { return }
                try? await Task.sleep(for: .seconds(2))
                if Task.isCancelled { return }
                autosaveNow()
            }
            .onChange(of: scenePhase) { _, phase in
                if phase != .active { autosaveNow() }
            }
            .onChange(of: autosaveEnabled) { _, on in
                if on {
                    autosaveFailedURL = nil
                    autosaveNow()
                }
            }
    }

    /// Toolbar: tap to undo; hold (or right-click) for Undo and Redo, like Pages.
    var undoButton: some View {
        Menu {
            Button {
                undoLastEdit()
            } label: {
                Label(edits.undoTitle, systemImage: "arrow.uturn.backward")
            }
            .disabled(!edits.canUndo)
            Button {
                redoLastEdit()
            } label: {
                Label(edits.redoTitle, systemImage: "arrow.uturn.forward")
            }
            .disabled(!edits.canRedo)
        } label: {
            Image(systemName: "arrow.uturn.backward")
        } primaryAction: {
            undoLastEdit()
        }
        .disabled(!edits.canUndo && !edits.canRedo)
        .accessibilityLabel(edits.undoTitle)
    }

    func undoLastEdit() {
        annotations.undoLast()
    }

    func redoLastEdit() {
        annotations.redoLast()
    }
}
