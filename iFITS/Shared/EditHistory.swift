//
//  EditHistory.swift
//  iFITS Start
//
//  One undo / redo history for the whole window: annotation and region edits go into the
//  window's UndoManager, so the toolbar's Undo button, ⌘Z / ⇧⌘Z, the Edit menu and the
//  three-finger swipes all undo the most recent edit, whatever kind it was.
//

import SwiftUI

@MainActor
@Observable
final class EditHistory {
    private(set) var canUndo = false
    private(set) var canRedo = false
    /// "Undo Move Region", "Redo Annotation", … (for the Undo button's menu).
    private(set) var undoTitle = "Undo"
    private(set) var redoTitle = "Redo"
    /// Goes up with every edit, undo and redo (autosave watches it).
    private(set) var editCount = 0

    /// The window's undo manager once SwiftUI provides it (a private one until then).
    @ObservationIgnored private(set) var manager = UndoManager()
    @ObservationIgnored private var observers: [NSObjectProtocol] = []

    /// One undoable step: what undo does, and what redo does.
    private struct Step {
        let name: String
        var undo: () -> Void
        var redo: () -> Void
    }
    /// Steps by id. The undo manager only stores the id, so nothing non-Sendable crosses into it.
    @ObservationIgnored private var steps: [UUID: Step] = [:]
    /// The newest step, for merging quick repeats (e.g. typing a region's name).
    @ObservationIgnored private var lastStep: (id: UUID, key: String, time: Date)?

    init() { observe() }

    /// Uses the window's undo manager (call when SwiftUI provides it). Steps recorded so far move
    /// over only as far as the new manager is concerned: the old ones are dropped.
    func attach(_ newManager: UndoManager?) {
        guard let newManager, newManager !== manager else { return }
        manager.removeAllActions(withTarget: self)
        steps = [:]
        lastStep = nil
        manager = newManager
        observe()
        refresh()
    }

    /// Records an edit that has already been made.
    /// - coalesce: steps with the same key less than 2 seconds apart become one step.
    func record(_ name: String, coalesce key: String? = nil,
                undo: @escaping () -> Void, redo: @escaping () -> Void) {
        let now = Date()
        if let key, let last = lastStep, last.key == key, now.timeIntervalSince(last.time) < 2,
           steps[last.id] != nil, manager.canUndo, manager.undoActionName == name {
            // Same kind of edit again: keep the first step's undo, take the new redo.
            steps[last.id]?.redo = redo
            lastStep = (last.id, key, now)
        } else {
            let id = push(Step(name: name, undo: undo, redo: redo))
            lastStep = key.map { (id, $0, now) }
        }
        editCount += 1
        refresh()
    }

    func undo() {
        if manager.canUndo { manager.undo() }
    }

    func redo() {
        if manager.canRedo { manager.redo() }
    }

    /// Forgets every step (a new file was opened).
    func clear() {
        manager.removeAllActions(withTarget: self)
        steps = [:]
        lastStep = nil
        refresh()
    }

    // MARK: Internals

    @discardableResult
    private func push(_ step: Step) -> UUID {
        let id = UUID()
        steps[id] = step
        manager.registerUndo(withTarget: self) { history in
            MainActor.assumeIsolated { history.run(id) }
        }
        manager.setActionName(step.name)
        return id
    }

    /// Undo (or redo) of step `id`: do it, and register the opposite step.
    private func run(_ id: UUID) {
        lastStep = nil
        guard let step = steps.removeValue(forKey: id) else { return }
        step.undo()
        push(Step(name: step.name, undo: step.redo, redo: step.undo))
        editCount += 1
    }

    private func observe() {
        observers.forEach { NotificationCenter.default.removeObserver($0) }
        observers = []
        for name in [Notification.Name.NSUndoManagerDidUndoChange, .NSUndoManagerDidRedoChange,
                     .NSUndoManagerDidCloseUndoGroup, .NSUndoManagerCheckpoint] {
            observers.append(NotificationCenter.default.addObserver(forName: name, object: manager, queue: .main) { [weak self] _ in
                MainActor.assumeIsolated { self?.refresh() }
            })
        }
    }

    private func refresh() {
        let u = manager.canUndo, r = manager.canRedo
        if canUndo != u { canUndo = u }
        if canRedo != r { canRedo = r }
        let ut = u ? manager.undoMenuItemTitle : "Undo"
        let rt = r ? manager.redoMenuItemTitle : "Redo"
        if undoTitle != ut { undoTitle = ut }
        if redoTitle != rt { redoTitle = rt }
    }
}
