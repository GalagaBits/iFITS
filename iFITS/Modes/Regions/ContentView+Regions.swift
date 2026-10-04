//
//  ContentView+Regions.swift
//  iFITS Start
//
//  Region taps, drags and pointer shapes, plus .reg import / export.
//

import SwiftUI

extension ContentView {
    // MARK: - Regions (R mode)

    var screenTransform: CGAffineTransform { imageToScreenTransform(in: viewportSize) }

    func layout(_ region: FITSRegion) -> RegionLayout {
        RegionLayout(region, imageToScreen: screenTransform, imageHeight: imageHeight)
    }

    func fitsPoint(atScreen p: CGPoint) -> CGPoint {
        RegionLayout.fitsPoint(p, screenTransform, imageHeight)
    }

    /// Size of a region dropped with a tap (or drawn with a tiny drag): about 80 points on screen.
    var defaultRegionPixels: CGFloat {
        80 / max(pointsPerImagePixel(in: viewportSize), 1e-9)
    }

    func applyDefaultSize(_ region: inout FITSRegion) {
        let d = defaultRegionPixels
        switch region.shape {
        case .ellipse, .rectangle: region.size = CGSize(width: d, height: d * 0.7)
        case .line: region.size = CGSize(width: d, height: 0)
        case .point: break
        }
    }

    /// What's under a screen point: a handle of the selected region (R mode only), or a region.
    func regionHit(atScreen p: CGPoint) -> RegionHit? {
        let reach: CGFloat = 16
        func distance(_ q: CGPoint) -> CGFloat { hypot(q.x - p.x, q.y - p.y) }

        if selectedMode == "R", let selected = regionStore.selected {
            let l = layout(selected)
            switch selected.shape {
            case .ellipse, .rectangle:
                if distance(l.rotationHandle) <= reach { return .rotate(selected.id) }
                if let handle = l.handles.min(by: { distance($0.point) < distance($1.point) }) {
                    let d = distance(handle.point)
                    // On a small region the handles crowd its inside; there, prefer moving it.
                    let preferMove = l.contains(p, tolerance: 0) && d > 8
                    if d <= reach, !preferMove {
                        return .resize(selected.id, sx: handle.sx, sy: handle.sy)
                    }
                }
            case .line:
                for (i, end) in l.lineEnds.enumerated() where distance(end) <= reach {
                    return .endpoint(selected.id, i)
                }
            case .point:
                break
            }
        }
        // Regions themselves: the selected one first, then from the newest (on top) down.
        var ordered = Array(regionStore.regions.reversed())
        if let i = ordered.firstIndex(where: { $0.id == regionStore.selectedID }) {
            ordered.insert(ordered.remove(at: i), at: 0)
        }
        return ordered.first(where: { layout($0).contains(p) }).map { .body($0.id) }
    }

    /// A tap (finger, pointer click, or Pencil outside A mode) on the image.
    func handleImageTap(atScreen p: CGPoint) {
        guard fitsImage != nil else { return }
        // R mode with a shape picked: a tap drops a region of the default size.
        if selectedMode == "R", let tool = regionStore.tool, regionHit(atScreen: p)?.isHandle != true {
            var region = FITSRegion(shape: tool, name: regionStore.nextName(), center: fitsPoint(atScreen: p),
                                    size: .zero, angle: 0, colorHex: regionStore.newRegionColor)
            applyDefaultSize(&region)
            regionStore.add(region)
            regionStore.tool = nil
            return
        }
        // Tapping a region selects it in any mode, and switches to R.
        if let hit = regionHit(atScreen: p) {
            regionStore.select(hit.regionID)
            if selectedMode != "R" { selectMode("R") }
        } else if selectedMode == "R" {
            regionStore.select(nil)
        }
    }

    /// Start of a one-finger / click drag. Returns true when the drag edits regions
    /// (R mode only); otherwise the drag moves the image as usual.
    func beginRegionDrag(atScreen p: CGPoint) -> Bool {
        guard selectedMode == "R", fitsImage != nil else { return false }
        let f = fitsPoint(atScreen: p)
        let hit = regionHit(atScreen: p)

        // A shape is picked: draw a new region (unless grabbing a handle of the selected one).
        if let tool = regionStore.tool, hit?.isHandle != true {
            let region = FITSRegion(shape: tool, name: regionStore.nextName(), center: f,
                                    size: .zero, angle: 0, colorHex: regionStore.newRegionColor)
            regionStore.add(region)
            regionDrag = RegionDrag(id: region.id, kind: .create(start: f), original: region)
            return true
        }

        guard let hit, let region = regionStore.region(hit.regionID) else { return false }
        regionStore.select(region.id)
        let kind: RegionDrag.Kind
        switch hit {
        case .body: kind = .move(start: f)
        case .resize(_, let sx, let sy): kind = .resize(sx: sx, sy: sy)
        case .endpoint(_, let index): kind = .endpoint(index)
        case .rotate: kind = .rotate
        }
        regionDrag = RegionDrag(id: region.id, kind: kind, original: region)
        return true
    }

    func continueRegionDrag(atScreen p: CGPoint) {
        guard let drag = regionDrag else { return }
        let updated = drag.region(draggedTo: fitsPoint(atScreen: p))
        regionStore.update(drag.id) { $0 = updated }
    }

    /// End of a region drag (`p` is nil when the drag was cancelled, e.g. by a pinch).
    func endRegionDrag(atScreen p: CGPoint?) {
        guard let drag = regionDrag else { return }
        if let p { continueRegionDrag(atScreen: p) }
        if case .create = drag.kind {
            // A tiny drag gets the default size, so the new region is easy to see and grab.
            if let region = regionStore.region(drag.id) {
                let l = layout(region)
                let tooSmall: Bool
                switch region.shape {
                case .ellipse, .rectangle: tooSmall = l.halfWidth < 4 || l.halfHeight < 4
                case .line: tooSmall = l.halfWidth < 4
                case .point: tooSmall = false
                }
                if tooSmall { regionStore.update(drag.id) { applyDefaultSize(&$0) } }
            }
            regionStore.tool = nil          // back to select / move
        }
        regionDrag = nil
    }

    /// Pointer shape (trackpad / mouse) at a screen point: arrows to stretch over handles,
    /// a curved arrow over the rotation handle, four arrows over a region, a crosshair
    /// while a shape is picked. nil = the normal pointer.
    func regionPointer(atScreen p: CGPoint) -> RegionPointer? {
        guard fitsImage != nil else { return nil }
        // Keep the shape for the whole drag, even when the pointer runs ahead of the handle.
        if let drag = regionDrag {
            guard let region = regionStore.region(drag.id) else { return nil }
            switch drag.kind {
            case .create: return .crosshair
            case .move: return .move
            case .rotate: return .rotate
            case .resize(let sx, let sy):
                return .stretching(alongDegrees: layout(region).stretchAxisDegrees(sx: sx, sy: sy))
            case .endpoint:
                return .stretching(alongDegrees: layout(region).stretchAxisDegrees(sx: 1, sy: 0))
            }
        }
        guard selectedMode == "R" else { return nil }
        switch regionHit(atScreen: p) {
        case .rotate?:
            return .rotate
        case .resize(let id, let sx, let sy)?:
            guard let region = regionStore.region(id) else { return nil }
            return .stretching(alongDegrees: layout(region).stretchAxisDegrees(sx: sx, sy: sy))
        case .endpoint(let id, _)?:
            guard let region = regionStore.region(id) else { return nil }
            return .stretching(alongDegrees: layout(region).stretchAxisDegrees(sx: 1, sy: 0))
        case .body?:
            return regionStore.tool == nil ? .move : .crosshair
        case nil:
            return regionStore.tool == nil ? nil : .crosshair
        }
    }

    // MARK: - Region files (.reg)

    /// Opens a DS9 .reg file and adds its regions to the current image.
    func importRegions() {
        guard fitsImage != nil else { return }
        importKind = .regions
        showPicker = true
    }

    func loadRegionFile(url: URL) {
        let accessing = url.startAccessingSecurityScopedResource()
        defer { if accessing { url.stopAccessingSecurityScopedResource() } }
        guard let data = try? Data(contentsOf: url) else {
            saveError = "Couldn't read \(url.lastPathComponent)."
            return
        }
        let parsed = DS9Regions.parse(String(decoding: data, as: UTF8.self), wcs: wcs, header: headerDict)
        guard !parsed.regions.isEmpty else {
            saveError = parsed.skipped > 0
                ? "\(url.lastPathComponent) only has shapes or coordinate systems iFITS can't show yet (it reads ellipse, circle, box, line and point regions in image, physical, fk5 or icrs coordinates)."
                : "No regions found in \(url.lastPathComponent)."
            return
        }
        regionStore.append(parsed.regions)
        selectMode("R")
        var message = "Loaded \(parsed.regions.count) region\(parsed.regions.count == 1 ? "" : "s")"
        if parsed.skipped > 0 { message += " (\(parsed.skipped) skipped)" }
        showSaveMessage(message)
    }

    /// Saves the regions as a DS9 .reg file (sky coordinates when the image has a celestial WCS).
    func exportRegions() {
        guard !regionStore.regions.isEmpty else {
            showSaveMessage("No regions to export")
            return
        }
        regionExportDocument = RegionFileDocument(
            text: DS9Regions.fileText(regionStore.regions, wcs: wcs, header: headerDict, fileName: fileName))
        showRegionExporter = true
    }
}
