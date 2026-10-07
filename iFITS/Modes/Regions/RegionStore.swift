//
//  RegionStore.swift
//  iFITS Start
//
//  All regions of the loaded image, the selection and the drawing tool.
//

import SwiftUI

/// All regions of the loaded image, plus what's selected and which drawing tool is on.
@MainActor
@Observable
final class RegionStore {
    var regions: [FITSRegion] = []
    var selectedID: UUID?
    /// Shape drawn by the next drag (or tap) on the image in R mode. nil = select / move.
    var tool: RegionShape?
    /// Region measured by the statistics boxes. nil = the entire image.
    var statsRegionID: UUID?
    /// Color for new regions (the last color picked).
    var newRegionColor = RegionColor.palette[0].hex
    /// The regions as of the last undo step (see ContentView+Undo). A change from this is a new
    /// step; undo and redo set both, so they never record a step of their own.
    @ObservationIgnored var recordedRegions: [FITSRegion] = []

    var selected: FITSRegion? {
        guard let selectedID else { return nil }
        return region(selectedID)
    }

    /// The region statistics are shown for (nil = entire image).
    var statsRegion: FITSRegion? {
        guard let statsRegionID, let r = region(statsRegionID), r.shape.hasStatistics else { return nil }
        return r
    }

    /// Regions that can be measured (everything but lines).
    var statsCandidates: [FITSRegion] { regions.filter { $0.shape.hasStatistics } }

    func region(_ id: UUID) -> FITSRegion? {
        regions.first { $0.id == id }
    }

    /// "Region 1", "Region 2", … (one more than the highest number in use).
    func nextName() -> String {
        let used = regions.compactMap { r -> Int? in
            guard r.name.hasPrefix("Region ") else { return nil }
            return Int(r.name.dropFirst(7))
        }
        return "Region \((used.max() ?? 0) + 1)"
    }

    func add(_ region: FITSRegion) {
        regions.append(region)
        select(region.id)
    }

    func update(_ id: UUID, _ change: (inout FITSRegion) -> Void) {
        guard let i = regions.firstIndex(where: { $0.id == id }) else { return }
        change(&regions[i])
    }

    /// Selecting a region also makes it the statistics region (unless it's a line).
    func select(_ id: UUID?) {
        selectedID = id
        if let id, let r = region(id), r.shape.hasStatistics { statsRegionID = id }
    }

    func delete(_ id: UUID) {
        regions.removeAll { $0.id == id }
        if selectedID == id { selectedID = nil }
        if statsRegionID == id { statsRegionID = nil }
    }

    func deleteSelected() {
        if let selectedID { delete(selectedID) }
    }

    /// Adds regions read from a file; unnamed ones get the next "Region N" name.
    func append(_ imported: [FITSRegion]) {
        for var r in imported {
            if r.name.isEmpty { r.name = nextName() }
            regions.append(r)
        }
    }

    /// Undo / redo: puts back an earlier set of regions.
    func restore(_ earlier: [FITSRegion]) {
        recordedRegions = earlier
        regions = earlier
        if let id = selectedID, region(id) == nil { selectedID = nil }
        if let id = statsRegionID, region(id) == nil { statsRegionID = nil }
    }

    func removeAll() {
        regions = []
        selectedID = nil
        statsRegionID = nil
        tool = nil
    }
}
