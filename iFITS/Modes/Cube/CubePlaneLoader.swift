//
//  CubePlaneLoader.swift
//  iFITS Start
//
//  Reads one plane (channel) of a cube from the file on demand.
//

import SwiftUI
import UIKit

/// The loaded cube: the file's bytes (memory-mapped) plus where each plane is.
nonisolated struct CubeSource: Sendable {
    let data: Data
    let width: Int
    let height: Int
    let bitpix: Int
    let dataOffset: Int
    let bscale: Double
    let bzero: Double
    let header: [String: String]
    /// NAXIS3, NAXIS4, …
    let axes: [CubeAxis]

    var planeCount: Int { axes.reduce(1) { $0 * $1.length } }

    /// nil unless the image has more than one plane.
    init?(data: Data, width: Int, height: Int, bitpix: Int, dataOffset: Int,
          bscale: Double, bzero: Double, axisLengths: [Int], header: [String: String]) {
        guard axisLengths.count >= 3, width > 0, height > 0,
              [8, 16, 32, 64, -32, -64].contains(bitpix) else { return nil }
        let axes = (3...axisLengths.count).map {
            CubeAxis(number: $0, length: axisLengths[$0 - 1], header: header)
        }
        guard axes.reduce(1, { $0 * $1.length }) > 1 else { return nil }
        self.data = data
        self.width = width
        self.height = height
        self.bitpix = bitpix
        self.dataOffset = dataOffset
        self.bscale = bscale
        self.bzero = bzero
        self.header = header
        self.axes = axes
    }

    /// Plane number for a channel index along each axis (FITS order: NAXIS3 varies fastest).
    func planeIndex(_ indices: [Int]) -> Int {
        var plane = 0, stride = 1
        for (axis, index) in zip(axes, indices) {
            plane += min(max(index, 0), axis.length - 1) * stride
            stride *= axis.length
        }
        return plane
    }

    /// One plane in physical units, rows flipped for display (like the first plane at load).
    func loadPlane(_ plane: Int) -> [Float]? {
        let n = width * height
        let bytes = abs(bitpix) / 8
        guard n > 0, plane >= 0, plane < planeCount else { return nil }
        let start = dataOffset + plane * n * bytes
        guard start >= 0, start + n * bytes <= data.count else { return nil }

        // Decode, physical values and row flip in one parallel pass (FITSPhysical.displayPixels).
        return data.withUnsafeBytes { (src: UnsafeRawBufferPointer) -> [Float]? in
            guard let base = src.baseAddress else { return nil }
            return FITSPhysical.displayPixels(from: base.advanced(by: start), bitpix: bitpix,
                                              width: width, height: height,
                                              bscale: bscale, bzero: bzero, header: header)
        }
    }
}

nonisolated struct CubePlaneRequest: Sendable {
    let source: CubeSource
    let plane: Int
    let settings: RenderSettings
    /// Clip percentile to apply to this plane (nil = keep the manual clip).
    let percentile: Double?
    /// Also colour the plane on the CPU (not needed while the GPU view draws the image).
    var drawImage = true
    /// Copies the plane to the GPU in the background (nil without Metal).
    var gpu: GPUUploader? = nil
    /// Planes that will most likely be wanted next, in order (read ahead into the cache).
    var readAhead: [Int] = []
}

nonisolated struct CubePlaneResult: @unchecked Sendable {
    let plane: Int
    let pixels: [Float]
    let stats: ImageStats
    /// Settings the image was drawn with (clip range from this plane when using a percentile).
    let settings: RenderSettings
    let image: UIImage?
    /// The plane already on the GPU (made in the background), or nil.
    let gpuImage: GPUImage?

    /// Memory this result holds, for the plane cache's budget.
    var bytes: Int {
        pixels.count * MemoryLayout<Float>.stride + (gpuImage?.bytes ?? 0)
            + stats.sorted.count * MemoryLayout<Float>.stride + (image == nil ? 0 : pixels.count * 4)
    }
}

/// Decodes, measures, uploads and (without the GPU view) draws cube planes off the main thread.
///
/// It works like a video player's buffer:
/// - Finished planes go into a cache (within a memory budget), so channels already seen or loaded
///   ahead show instantly.
/// - A queue of upcoming planes (the next playback frames) is loaded ahead, up to two planes at a
///   time, as far ahead as the memory budget allows.
/// - The plane asked for always goes first. Only the newest plane asked for is shown, so fast
///   scrubbing skips old ones.
@MainActor
final class CubePlaneLoader {
    var onPlane: ((CubePlaneResult) -> Void)?

    /// Planes read at the same time. Each read already uses every core for its heavy steps, so two
    /// is enough to keep a steady stream without slowing the plane asked for.
    private let maxLoads = 2
    private var generation = 0
    /// The plane last asked for. Only it is shown; other planes finishing are just cached.
    private var wanted: Int?
    /// The last request, copied for reading ahead (same file and settings).
    private var template: CubePlaneRequest?
    /// Upcoming planes to load ahead, in the order they'll be shown.
    private var queue: [Int] = []
    /// Planes being read right now.
    private var loading: Set<Int> = []
    /// Planes that couldn't be read (playback skips them instead of waiting forever).
    private var failed: Set<Int> = []

    // The cache: planes by number, least recently used first in `recent`.
    private var cache: [Int: CubePlaneResult] = [:]
    private var recent: [Int] = []
    private var cacheBytes = 0
    /// Memory one plane takes in the cache (measured from the first one read).
    private var planeBytes = 0
    /// At most a sixth of the device's memory, and never more than 1 GB.
    private let cacheBudget = min(1 << 30, Int(ProcessInfo.processInfo.physicalMemory / 6))

    init() {
        // Give the memory back if iPadOS says it's running low.
        _ = NotificationCenter.default.addObserver(forName: UIApplication.didReceiveMemoryWarningNotification,
                                                   object: nil, queue: .main) { [weak self] _ in
            MainActor.assumeIsolated { self?.clearCache() }
        }
    }

    /// Forget anything in progress and every cached plane (a new file was opened).
    func reset() {
        generation += 1
        wanted = nil
        template = nil
        queue = []
        loading = []
        failed = []
        planeBytes = 0
        clearCache()
    }

    /// Show `request.plane` (now if it's cached), and load `request.readAhead` behind it.
    func request(_ request: CubePlaneRequest) {
        wanted = request.plane
        template = request
        queue = request.readAhead
        if let hit = cache[request.plane] {
            touch(request.plane)
            onPlane?(hit)
        }
        pump()
    }

    /// New upcoming planes to load ahead (e.g. when Play is pressed), without changing the plane
    /// shown. `template` says which file and settings to read them with.
    func setReadAhead(_ planes: [Int], template: CubePlaneRequest) {
        self.template = template
        queue = planes
        pump()
    }

    /// Whether `plane` can be shown right away. Playback waits for this before moving on, like a
    /// video player buffering. True when loading ahead isn't possible (planes too big for the
    /// cache) or the plane can't be read, so playback never waits forever.
    func readyToShow(_ plane: Int) -> Bool {
        cache[plane] != nil || failed.contains(plane) || planesThatFit < 2
    }

    /// How many planes fit in the cache (unlimited until the first plane has been measured).
    private var planesThatFit: Int { planeBytes > 0 ? cacheBudget / planeBytes : Int.max }

    /// How many upcoming planes to load ahead: the queue, as far as the cache has room next to the
    /// plane shown.
    private var aheadLimit: Int { max(0, min(queue.count, planesThatFit - 1)) }

    /// Starts loads until `maxLoads` are running: the plane asked for first, then upcoming planes.
    private func pump() {
        guard let template else { return }
        var candidates: [Int] = []
        if let wanted { candidates.append(wanted) }
        candidates += queue.prefix(aheadLimit)
        for plane in candidates where loading.count < maxLoads {
            guard plane >= 0, plane < template.source.planeCount, cache[plane] == nil,
                  !loading.contains(plane), !failed.contains(plane) else { continue }
            start(CubePlaneRequest(source: template.source, plane: plane, settings: template.settings,
                                   percentile: template.percentile, drawImage: template.drawImage,
                                   gpu: template.gpu, readAhead: []))
        }
    }

    private func start(_ request: CubePlaneRequest) {
        loading.insert(request.plane)
        let gen = generation
        Task.detached(priority: .userInitiated) {
            let result = CubePlaneLoader.load(request)
            await self.finish(result, plane: request.plane, generation: gen)
        }
    }

    private func finish(_ result: CubePlaneResult?, plane: Int, generation gen: Int) {
        guard gen == generation else { return }         // from a file that's no longer open
        loading.remove(plane)
        if let result {
            store(result)
            if result.plane == wanted { onPlane?(result) }
        } else {
            failed.insert(plane)
        }
        pump()
    }

    private func store(_ result: CubePlaneResult) {
        if let old = cache[result.plane] { cacheBytes -= old.bytes }
        cache[result.plane] = result
        cacheBytes += result.bytes
        planeBytes = max(planeBytes, result.bytes)
        touch(result.plane)
        // Drop the least recently used planes until the cache fits. Never the one shown, the one
        // just read, or the upcoming planes being buffered.
        let keep = Set(queue.prefix(aheadLimit)).union([result.plane]).union(wanted.map { [$0] } ?? [])
        while cacheBytes > cacheBudget, let victim = recent.first(where: { !keep.contains($0) }) {
            remove(victim)
        }
    }

    private func touch(_ plane: Int) {
        recent.removeAll { $0 == plane }
        recent.append(plane)
    }

    private func remove(_ plane: Int) {
        if let old = cache.removeValue(forKey: plane) { cacheBytes -= old.bytes }
        recent.removeAll { $0 == plane }
    }

    private func clearCache() {
        cache.removeAll()
        recent.removeAll()
        cacheBytes = 0
    }

    nonisolated static func load(_ request: CubePlaneRequest) -> CubePlaneResult? {
        let source = request.source
        guard let pixels = source.loadPlane(request.plane) else { return nil }
        let stats = ImageStats(values: pixels, width: source.width)
        var settings = request.settings
        if let p = request.percentile {
            (settings.clipMin, settings.clipMax) = stats.clipRange(percentile: p)
        }
        let image = request.drawImage
            ? FITSRenderer.render(pixels, width: source.width, height: source.height, settings: settings)
            : nil
        // Copied to the GPU here, off the main thread (it used to happen on the main thread).
        let gpuImage = request.gpu?.upload(pixels: pixels, width: source.width, height: source.height)
        return CubePlaneResult(plane: request.plane, pixels: pixels, stats: stats, settings: settings,
                               image: image, gpuImage: gpuImage)
    }
}
