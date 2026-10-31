//
//  MetalImageRenderer.swift
//  iFITS Start
//
//  Draws the 2-D image with Metal (the GPU) instead of colouring every pixel on the CPU.
//
//  Why this is faster: the CPU renderer (FITSRenderer) recolours every pixel and builds a brand-new
//  picture each time a setting changes, then SwiftUI sends that whole picture to the GPU again. For
//  an 8000 × 8000 image that's 64 million pixels per slider step. Here the raw pixel values are sent
//  to the GPU once (per image, or per cube channel) and stay there. A setting change only:
//    - updates two numbers (clip min and the table scale), or
//    - rebuilds the 4096-entry colour table (scaling + colormap), which is tiny,
//  and the GPU recolours just the pixels that are on screen, in parallel, when it draws.
//
//  The colour table is built by FITSRenderer.colorTable, the same code the CPU renderer uses, so
//  the GPU and CPU (exports, fallback) give the same colours.
//

import SwiftUI
import UIKit
import Metal
import MetalKit
import QuartzCore
import CoreGraphics
import Observation
import simd

/// Same layout as ImageUniforms in ImageShaders.metal. The order and types must match exactly.
struct ImageUniforms {
    var viewSize: SIMD2<Float>
    var translation: SIMD2<Float>
    var pointsPerPixel: Float
    var contentScale: Float
    var clipMin: Float
    var tableScale: Float
    var tileRect: SIMD4<Float>
    var screenRect: SIMD4<Float>
}

/// One piece of the image on the GPU. Most images are a single tile; an image wider or taller
/// than the GPU's largest texture is split into several.
private struct ImageTile {
    let texture: MTLTexture
    let x: Int
    let y: Int
    let width: Int
    let height: Int
}

/// The settings that change the colour table (the clip range doesn't: it's just two numbers).
private struct TableKey: Equatable {
    let scaling: ScalingType
    let alpha: Double
    let gamma: Double
    let colormap: Colormap
    let inverted: Bool

    init(_ s: RenderSettings) {
        scaling = s.scaling
        alpha = s.alpha
        gamma = s.gamma
        colormap = s.colormap
        inverted = s.inverted
    }
}

@Observable
final class MetalImageRenderer {
    /// True while the GPU holds the current image and draws it. When false (no Metal, or the image
    /// couldn't be uploaded), ContentView falls back to the CPU renderer.
    private(set) var isReady = false
    /// Goes up each time new pixels are uploaded, so the view knows to redraw.
    private(set) var revision = 0

    private let device: MTLDevice?
    private let queue: MTLCommandQueue?
    private let pipeline: MTLRenderPipelineState?

    @ObservationIgnored private var tiles: [ImageTile] = []
    @ObservationIgnored private var tableTexture: MTLTexture?
    @ObservationIgnored private var tableKey: TableKey?
    @ObservationIgnored private var settings = RenderSettings()

    /// Metal is set up and can draw (false on a device without Metal, or if the shaders are missing).
    var isAvailable: Bool { pipeline != nil }

    init() {
        let device = MTLCreateSystemDefaultDevice()
        self.device = device
        self.queue = device?.makeCommandQueue()

        // The two functions in ImageShaders.metal, drawing into the view's normal colour format.
        var pipeline: MTLRenderPipelineState? = nil
        if let device, let library = device.makeDefaultLibrary(),
           let vertex = library.makeFunction(name: "imageVertex"),
           let fragment = library.makeFunction(name: "imageFragment") {
            let d = MTLRenderPipelineDescriptor()
            d.vertexFunction = vertex
            d.fragmentFunction = fragment
            d.colorAttachments[0].pixelFormat = .bgra8Unorm
            pipeline = try? device.makeRenderPipelineState(descriptor: d)
        }
        self.pipeline = pipeline
    }

    // MARK: Uploading the image

    /// The largest texture side this GPU allows (16384 on every iPad and iPhone that runs iOS 26).
    private var maxTextureSide: Int {
        guard let device else { return 8192 }
        return device.supportsFamily(.apple3) ? 16384 : 8192
    }

    /// Sends the raw pixel values (row 0 = top, as FITSRenderer stores them) to the GPU.
    /// Called once per image and once per cube channel; nothing is uploaded while you change settings.
    /// New textures are made each time (the old ones are freed once the GPU is done with them),
    /// so a frame that's still being drawn never sees half-replaced pixels.
    func load(pixels: [Float], width: Int, height: Int) {
        guard isAvailable, let device, width > 0, height > 0, pixels.count >= width * height else {
            unload()
            return
        }
        let side = maxTextureSide
        var newTiles: [ImageTile] = []
        var failed = false

        pixels.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { failed = true; return }
            // Tiles of at most side × side pixels, left to right, top to bottom.
            var y = 0
            while y < height, !failed {
                let h = min(side, height - y)
                var x = 0
                while x < width, !failed {
                    let w = min(side, width - x)
                    // One 32-bit float per pixel (the raw value, not a colour).
                    let d = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .r32Float,
                                                                     width: w, height: h, mipmapped: false)
                    d.usage = .shaderRead
                    d.storageMode = .shared
                    if let texture = device.makeTexture(descriptor: d) {
                        // Copy this tile's part of each row (rows are `width` pixels apart in memory).
                        let start = base.advanced(by: (y * width + x) * MemoryLayout<Float>.stride)
                        texture.replace(region: MTLRegionMake2D(0, 0, w, h), mipmapLevel: 0,
                                        withBytes: start, bytesPerRow: width * MemoryLayout<Float>.stride)
                        newTiles.append(ImageTile(texture: texture, x: x, y: y, width: w, height: h))
                    } else {
                        failed = true     // out of GPU memory: use the CPU renderer instead
                    }
                    x += w
                }
                y += h
            }
        }

        if failed || newTiles.isEmpty {
            unload()
            return
        }
        tiles = newTiles
        revision += 1
        if !isReady { isReady = true }
    }

    /// Forgets the image (the CPU renderer takes over).
    func unload() {
        tiles = []
        if isReady { isReady = false }
    }

    // MARK: Settings

    /// New render settings. Rebuilds the small colour table only when the scaling or colormap
    /// changed; a new clip range is just new numbers for the shader.
    func apply(_ newSettings: RenderSettings) {
        settings = newSettings
        let key = TableKey(newSettings)
        guard key != tableKey || tableTexture == nil, let device else { return }
        let table = FITSRenderer.colorTable(for: newSettings)       // bytes R, G, B, A per entry
        let d = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm, width: table.count,
                                                         height: 1, mipmapped: false)
        d.usage = .shaderRead
        guard let texture = device.makeTexture(descriptor: d) else { return }
        table.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { return }
            texture.replace(region: MTLRegionMake2D(0, 0, table.count, 1), mipmapLevel: 0,
                            withBytes: base, bytesPerRow: table.count * 4)
        }
        // A new texture rather than overwriting the old one, for the same reason as in load().
        tableTexture = texture
        tableKey = key
    }

    // MARK: Drawing

    /// Draws the image into `view`. `imageToScreen` is ContentView.imageToScreenTransform: image
    /// pixel → screen point, the same transform the annotations, regions and grid use, so they
    /// stay lined up with the image.
    func draw(in view: MTKView, imageToScreen t: CGAffineTransform) {
        guard let pipeline, let queue,
              let pass = view.currentRenderPassDescriptor,
              let commandBuffer = queue.makeCommandBuffer() else { return }
        // Clear to transparent: outside the image (and NaN pixels) the black background shows.
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 0)
        guard let encoder = commandBuffer.makeRenderCommandEncoder(descriptor: pass) else { return }

        let bounds = view.bounds.size
        if let tableTexture, t.a > 0, bounds.width > 0, bounds.height > 0 {
            encoder.setRenderPipelineState(pipeline)
            encoder.setFragmentTexture(tableTexture, index: 1)

            // Clip range → index into the colour table (exactly as FITSRenderer.render does it).
            let lo = Float(settings.clipMin), hi = Float(settings.clipMax)
            let span = hi - lo
            let lastIndex = Float(tableTexture.width - 1)
            let tableScale: Float = span > 0 ? lastIndex / span : 0

            let k = t.a                                         // points per image pixel
            for tile in tiles {
                // Where this tile lands on screen, cut to the visible part (skipped if off screen).
                let rect = CGRect(x: t.tx + k * CGFloat(tile.x), y: t.ty + k * CGFloat(tile.y),
                                  width: k * CGFloat(tile.width), height: k * CGFloat(tile.height))
                    .intersection(CGRect(origin: .zero, size: bounds))
                guard !rect.isNull, rect.width > 0, rect.height > 0 else { continue }

                var u = ImageUniforms(
                    viewSize: SIMD2(Float(bounds.width), Float(bounds.height)),
                    translation: SIMD2(Float(t.tx), Float(t.ty)),
                    pointsPerPixel: Float(k),
                    contentScale: Float(view.drawableSize.width / bounds.width),
                    clipMin: lo,
                    tableScale: tableScale,
                    tileRect: SIMD4(Float(tile.x), Float(tile.y), Float(tile.width), Float(tile.height)),
                    screenRect: SIMD4(Float(rect.minX), Float(rect.minY), Float(rect.maxX), Float(rect.maxY)))
                encoder.setVertexBytes(&u, length: MemoryLayout<ImageUniforms>.stride, index: 0)
                encoder.setFragmentBytes(&u, length: MemoryLayout<ImageUniforms>.stride, index: 0)
                encoder.setFragmentTexture(tile.texture, index: 0)
                encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)
            }
        }
        encoder.endEncoding()
        if let drawable = view.currentDrawable { commandBuffer.present(drawable) }
        commandBuffer.commit()
    }

    /// The Metal device, for the view to use the same GPU.
    var metalDevice: MTLDevice? { device }
}

/// The MTKView that shows the image. It fills the image area without being scaled by SwiftUI:
/// zoom and pan are done in the shader, so the image stays sharp at every zoom level.
struct MetalImageView: UIViewRepresentable {
    let renderer: MetalImageRenderer
    let settings: RenderSettings
    /// Image pixel → screen point (ContentView.imageToScreenTransform).
    let transform: CGAffineTransform
    /// MetalImageRenderer.revision: changes when new pixels were uploaded.
    let revision: Int

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> MTKView {
        let view = MTKView(frame: .zero, device: renderer.metalDevice)
        view.colorPixelFormat = .bgra8Unorm
        view.depthStencilPixelFormat = .invalid
        view.framebufferOnly = true
        // Transparent where there's no image, so the black image area shows through.
        view.isOpaque = false
        view.backgroundColor = .clear
        view.layer.isOpaque = false
        // The colours are sRGB, like the CPU renderer's pictures.
        (view.layer as? CAMetalLayer)?.colorspace = CGColorSpace(name: CGColorSpace.sRGB)
        // Draw only when something changed (setNeedsDisplay), not 60 times a second.
        view.isPaused = true
        view.enableSetNeedsDisplay = true
        // Touches go to the gesture and annotation layers on top.
        view.isUserInteractionEnabled = false
        context.coordinator.renderer = renderer
        view.delegate = context.coordinator
        return view
    }

    func updateUIView(_ view: MTKView, context: Context) {
        let c = context.coordinator
        c.renderer = renderer
        c.transform = transform
        // Full Retina sharpness.
        let scale = max(1, view.traitCollection.displayScale)
        if view.contentScaleFactor != scale { view.contentScaleFactor = scale }
        // Redraw only for real changes (SwiftUI also calls this for unrelated updates).
        let state = Coordinator.DrawState(settings: settings, transform: transform, revision: revision)
        guard state != c.lastState else { return }
        c.lastState = state
        renderer.apply(settings)
        view.setNeedsDisplay()
    }

    final class Coordinator: NSObject, @preconcurrency MTKViewDelegate {
        struct DrawState: Equatable {
            let settings: RenderSettings
            let transform: CGAffineTransform
            let revision: Int
        }

        var renderer: MetalImageRenderer?
        var transform: CGAffineTransform = .identity
        var lastState: DrawState?

        func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
            view.setNeedsDisplay()
        }

        func draw(in view: MTKView) {
            renderer?.draw(in: view, imageToScreen: transform)
        }
    }
}
