//
//  FITSRenderer.swift
//  iFITS Start
//
//  Turns pixel values into a colored image.
//

import SwiftUI
import UIKit

private struct RenderedImage: @unchecked Sendable { let image: UIImage? }

/// Raw buffers shared by the parallel render loop. Each chunk only touches its own range of
/// `out`, so this is safe to share across threads.
nonisolated private struct RenderBuffers: @unchecked Sendable {
    let src: UnsafePointer<Float>
    let table: UnsafePointer<UInt32>
    let out: UnsafeMutablePointer<UInt32>
}

/// Turns raw FITS pixels into a colored image. Runs one render at a time off the main
/// thread; while one is running, only the newest request is kept, so dragging a slider
/// or clip line updates as fast as the device can draw without piling up work.
@MainActor
final class FITSRenderer {
    var onImage: ((UIImage?) -> Void)?

    private var pixels: [Float] = []
    private var width = 0
    private var height = 0
    private var busy = false
    private var pending: RenderSettings?
    private var lastRequested: RenderSettings?
    private var generation = 0

    /// The loaded pixels (physical units, display order: row 0 = top), for statistics.
    var pixelData: (pixels: [Float], width: Int, height: Int) { (pixels, width, height) }

    /// Pixel value (physical units) at a 0-based column/row of the loaded image.
    func value(column: Int, row: Int) -> Float? {
        guard column >= 0, row >= 0, column < width, row < height, row * width + column < pixels.count else { return nil }
        return pixels[row * width + column]
    }

    func load(_ pixels: [Float], width: Int, height: Int, renderedWith settings: RenderSettings) {
        self.pixels = pixels
        self.width = width
        self.height = height
        generation += 1
        pending = nil
        lastRequested = settings
    }

    func request(_ settings: RenderSettings) {
        guard !pixels.isEmpty, settings != lastRequested else { return }
        lastRequested = settings
        if busy { pending = settings } else { start(settings) }
    }

    private func start(_ settings: RenderSettings) {
        busy = true
        let data = pixels, w = width, h = height, gen = generation
        Task.detached(priority: .userInitiated) {
            let result = RenderedImage(image: FITSRenderer.render(data, width: w, height: h, settings: settings))
            await self.finish(result, generation: gen)
        }
    }

    private func finish(_ result: RenderedImage, generation gen: Int) {
        busy = false
        if gen == generation { onImage?(result.image) }
        if let next = pending {
            pending = nil
            start(next)
        }
    }

    nonisolated static func render(_ data: [Float], width: Int, height: Int, settings s: RenderSettings) -> UIImage? {
        let count = width * height
        guard count > 0, data.count >= count else { return nil }

        // Fold scaling + colormap into one 4096-entry table indexed by the linear clip position.
        let colors = s.colormap.lut(inverted: s.inverted)
        let tableSize = 4096
        var table = [UInt32](repeating: 0, count: tableSize)
        for i in 0..<tableSize {
            let x = Double(i) / Double(tableSize - 1)
            let y = s.scaling.apply(x, alpha: s.alpha, gamma: s.gamma)
            let yc = y.isFinite ? min(1, max(0, y)) : 0
            table[i] = colors[Int((yc * 255).rounded())]
        }

        let lo = Float(s.clipMin), hi = Float(s.clipMax)
        let span = hi - lo
        let k: Float = span > 0 ? Float(tableSize - 1) / span : 0
        let maxIndex = Float(tableSize - 1)
        let nanColor: UInt32 = 0   // NaN pixels are transparent, so the black background shows

        let byteCount = count * 4
        let raw = UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: 16)
        let out = raw.bindMemory(to: UInt32.self, capacity: count)
        let chunks = max(1, min(32, count / 32_768))

        data.withUnsafeBufferPointer { src in
            table.withUnsafeBufferPointer { tb in
                guard let srcBase = src.baseAddress, let tableBase = tb.baseAddress else { return }
                // Each chunk writes its own slice of `out`, so sharing the pointers is safe.
                let buffers = RenderBuffers(src: srcBase, table: tableBase, out: out)
                DispatchQueue.concurrentPerform(iterations: chunks) { c in
                    let start = c * count / chunks
                    let end = (c + 1) * count / chunks
                    for i in start..<end {
                        let v = buffers.src[i]
                        if v.isNaN { buffers.out[i] = nanColor; continue }
                        var f = (v - lo) * k
                        if !(f > 0) { f = 0 } else if f > maxIndex { f = maxIndex }
                        buffers.out[i] = buffers.table[Int(f)]
                    }
                }
            }
        }

        guard let provider = CGDataProvider(dataInfo: nil, data: raw, size: byteCount,
                                            releaseData: { _, ptr, _ in ptr.deallocate() }) else {
            raw.deallocate()
            return nil
        }
        return makeImage(provider: provider, width: width, height: height)
    }

    nonisolated static func makeImage(pixels: [UInt32], width: Int, height: Int) -> UIImage? {
        let data = pixels.withUnsafeBufferPointer { Data(buffer: $0) }
        guard let provider = CGDataProvider(data: data as CFData) else { return nil }
        return makeImage(provider: provider, width: width, height: height)
    }

    nonisolated private static func makeImage(provider: CGDataProvider, width: Int, height: Int) -> UIImage? {
        guard let cg = CGImage(width: width, height: height,
                               bitsPerComponent: 8, bitsPerPixel: 32, bytesPerRow: width * 4,
                               space: CGColorSpace(name: CGColorSpace.sRGB) ?? CGColorSpaceCreateDeviceRGB(),
                               bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue),
                               provider: provider, decode: nil,
                               shouldInterpolate: false, intent: .defaultIntent) else { return nil }
        return UIImage(cgImage: cg)
    }
}
