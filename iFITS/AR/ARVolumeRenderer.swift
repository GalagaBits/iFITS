//
//  ARVolumeRenderer.swift
//  iFITS Start
//
//  Draws the cube with Metal: as a 3-D object on a dark background, or (camera mode) in the
//  room through ARKit. Also draws the 3-D grid and works out where its labels go on screen.
//

import MetalKit
import ARKit
import simd

/// Same layout as VolumeUniforms in ARShaders.metal.
struct ARVolumeUniforms {
    var inverseMVP: simd_float4x4
    var boxMin: SIMD4<Float>
    var boxSize: SIMD4<Float>
    var params: SIMD4<Float>
    var dims: SIMD4<UInt32>
}

/// Same layout as LineVertex in ARShaders.metal.
struct ARLineVertex {
    var position: SIMD4<Float>
    var color: SIMD4<Float>
}

/// Same layout as CameraVertex in ARShaders.metal.
private struct ARCameraVertex {
    var position: SIMD2<Float>
    var texCoord: SIMD2<Float>
}

/// Keeps camera textures alive until the GPU has finished with them.
private final class TextureHolder: @unchecked Sendable {
    let textures: [CVMetalTexture]
    init(_ textures: [CVMetalTexture]) { self.textures = textures }
}

final class ARVolumeRenderer: NSObject {
    private let device: MTLDevice
    private let queue: MTLCommandQueue
    private let volumePipeline: MTLRenderPipelineState
    private let linePipeline: MTLRenderPipelineState
    private let cameraPipeline: MTLRenderPipelineState
    private var textureCache: CVMetalTextureCache?
    private weak var view: MTKView?

    let session = ARSession()

    // Data
    private var volumeTexture: MTLTexture?
    private var colormapTexture: MTLTexture?
    private var dims = SIMD3<Int>(1, 1, 1)
    /// Original pixels / channels the box covers (sets its proportions).
    private var extent = SIMD3<Float>(1, 1, 1)
    private var axes: ARAxisSet?
    private var lineBuffer: MTLBuffer?
    private var lineVertexCount = 0

    // Settings
    private var settings: ARRenderSettings?
    /// Box size in metres (also used, unscaled, for the on-screen 3-D view).
    private var boxSize = SIMD3<Float>(0.3, 0.3, 0.3)

    // View
    private static let defaultRotation = simd_quatf(angle: 0.35, axis: SIMD3<Float>(1, 0, 0))
        * simd_quatf(angle: -0.55, axis: SIMD3<Float>(0, 1, 0))
    private static let upright = simd_quatf(angle: 0, axis: SIMD3<Float>(0, 1, 0))
    private var rotation = ARVolumeRenderer.defaultRotation
    /// While a finger is moving the cube, the 3-D view draws at 1× so it keeps up.
    private var interacting = false
    private var zoom: Float = 1
    /// Where the cube sits in the room (camera mode): position and the turn that faces the camera.
    private var placement: simd_float4x4?
    private var placedOnSurface = false
    private(set) var inRoom = false

    /// Labels for the grid, in view points, after each frame.
    var onLabels: (([ARScreenLabel]) -> Void)?
    /// Messages for the person (tracking, camera problems); nil clears it.
    var onStatus: ((String?) -> Void)?
    /// Camera mode couldn't start (or stopped).
    var onRoomFailed: (() -> Void)?

    init?(view: MTKView) {
        guard let device = view.device ?? MTLCreateSystemDefaultDevice(),
              let queue = device.makeCommandQueue(),
              let library = device.makeDefaultLibrary() else { return nil }
        self.device = device
        self.queue = queue

        func pipeline(_ vertex: String, _ fragment: String, blended: Bool) -> MTLRenderPipelineState? {
            guard let v = library.makeFunction(name: vertex), let f = library.makeFunction(name: fragment) else { return nil }
            let d = MTLRenderPipelineDescriptor()
            d.vertexFunction = v
            d.fragmentFunction = f
            d.colorAttachments[0].pixelFormat = .bgra8Unorm
            if blended {
                // Premultiplied alpha "over".
                let c = d.colorAttachments[0]!
                c.isBlendingEnabled = true
                c.rgbBlendOperation = .add
                c.alphaBlendOperation = .add
                c.sourceRGBBlendFactor = .one
                c.sourceAlphaBlendFactor = .one
                c.destinationRGBBlendFactor = .oneMinusSourceAlpha
                c.destinationAlphaBlendFactor = .oneMinusSourceAlpha
            }
            return try? device.makeRenderPipelineState(descriptor: d)
        }
        guard let vp = pipeline("arFullscreenVertex", "arVolumeFragment", blended: true),
              let lp = pipeline("arLineVertex", "arLineFragment", blended: true),
              let cp = pipeline("arCameraVertex", "arCameraFragment", blended: false) else { return nil }
        volumePipeline = vp
        linePipeline = lp
        cameraPipeline = cp
        var cache: CVMetalTextureCache?
        CVMetalTextureCacheCreate(nil, nil, device, nil, &cache)
        textureCache = cache
        super.init()

        self.view = view
        view.device = device
        view.colorPixelFormat = .bgra8Unorm
        view.depthStencilPixelFormat = .invalid
        view.framebufferOnly = true
        view.clearColor = MTLClearColor(red: 0.02, green: 0.025, blue: 0.035, alpha: 1)
        view.contentScaleFactor = Self.restingScale
        // The 3-D view only draws when something changes; camera mode draws continuously.
        view.isPaused = true
        view.enableSetNeedsDisplay = true
        view.delegate = self
        session.delegate = self
    }

    // MARK: Data

    /// Uploads the cube (raw values, 32-bit float) as a 3-D texture.
    func setVolume(_ volume: ARVolume) {
        let d = MTLTextureDescriptor()
        d.textureType = .type3D
        d.pixelFormat = .r32Float
        d.width = volume.width
        d.height = volume.height
        d.depth = volume.depth
        d.usage = .shaderRead
        d.storageMode = .shared
        guard let texture = device.makeTexture(descriptor: d) else {
            onStatus?("The cube is too big for this iPad's GPU.")
            return
        }
        volume.values.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { return }
            texture.replace(region: MTLRegionMake3D(0, 0, 0, volume.width, volume.height, volume.depth),
                            mipmapLevel: 0, slice: 0, withBytes: base,
                            bytesPerRow: volume.width * 4, bytesPerImage: volume.width * volume.height * 4)
        }
        volumeTexture = texture
        dims = SIMD3(volume.width, volume.height, volume.depth)
        extent = SIMD3(Float(volume.extentX), Float(volume.extentY), Float(volume.extentZ))
        updateBox()
    }

    func setAxes(_ axes: ARAxisSet?) {
        self.axes = axes
        rebuildLines()
    }

    /// New settings from SwiftUI (colormap, range, grid, camera mode, …).
    func apply(_ new: ARRenderSettings) {
        let old = settings
        settings = new
        if old?.colormap != new.colormap || old?.inverted != new.inverted {
            makeColormapTexture(new.colormap, inverted: new.inverted)
        }
        if old?.depthStretch != new.depthStretch {
            updateBox()
        }
        if let old, old.resetToken != new.resetToken {
            resetView()
        }
        if old?.inRoom != new.inRoom {
            setInRoom(new.inRoom)
        }
        redraw()
    }

    private func makeColormapTexture(_ colormap: Colormap, inverted: Bool) {
        let lut = colormap.lut(inverted: inverted)          // bytes R, G, B, A
        let d = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm, width: lut.count,
                                                         height: 1, mipmapped: false)
        d.usage = .shaderRead
        guard let texture = device.makeTexture(descriptor: d) else { return }
        lut.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { return }
            texture.replace(region: MTLRegionMake2D(0, 0, lut.count, 1), mipmapLevel: 0,
                            withBytes: base, bytesPerRow: lut.count * 4)
        }
        colormapTexture = texture
    }

    /// Box proportions: image width × height, and depth as long as the longer side (× stretch).
    /// The longest side is 0.3 m.
    private func updateBox() {
        let stretch = settings?.depthStretch ?? 1
        let raw = SIMD3<Float>(extent.x, extent.y, max(extent.x, extent.y) * stretch)
        boxSize = raw / max(raw.max(), 1e-6) * 0.3
        rebuildLines()
    }

    // MARK: View control (gestures)

    func rotate(dx: Float, dy: Float) {
        rotation = simd_quatf(angle: dx * 0.008, axis: SIMD3<Float>(0, 1, 0)) * rotation
        rotation = simd_quatf(angle: dy * 0.008, axis: SIMD3<Float>(1, 0, 0)) * rotation
        rotation = simd_normalize(rotation)
        redraw()
    }

    /// Two-finger twist: turns the cube about the line of sight.
    func twist(by angle: Float) {
        rotation = simd_normalize(simd_quatf(angle: -angle, axis: SIMD3<Float>(0, 0, 1)) * rotation)
        redraw()
    }

    func zoom(by factor: Float) {
        zoom = min(max(zoom * factor, 0.15), 12)
        redraw()
    }

    func resetView() {
        rotation = inRoom ? Self.upright : Self.defaultRotation
        zoom = 1
        if inRoom {
            placement = nil
            placedOnSurface = false
        }
        redraw()
    }

    /// Drawing resolution (× points) in the 3-D view when nothing is moving.
    private static let restingScale: CGFloat = 1.5

    /// A gesture started or ended: draw at 1× while it lasts, then sharpen.
    func setInteracting(_ on: Bool) {
        guard on != interacting, let view else { return }
        interacting = on
        if !inRoom {
            view.contentScaleFactor = on ? 1 : Self.restingScale
            view.setNeedsDisplay()
        }
    }

    private func redraw() {
        guard let view, view.isPaused else { return }
        view.setNeedsDisplay()
    }

    // MARK: Camera mode (ARKit)

    private func setInRoom(_ on: Bool) {
        guard let view else { return }
        if on {
            guard ARWorldTrackingConfiguration.isSupported else {
                onStatus?("This iPad can't place objects in the room.")
                onRoomFailed?()
                return
            }
            // Without a camera usage description iPadOS ends the app as soon as the camera starts.
            guard Bundle.main.object(forInfoDictionaryKey: "NSCameraUsageDescription") != nil else {
                onStatus?("To use the camera, add \u{201C}Privacy - Camera Usage Description\u{201D} to the iFITS target's Info in Xcode.")
                onRoomFailed?()
                return
            }
            let config = ARWorldTrackingConfiguration()
            config.planeDetection = [.horizontal]
            session.run(config, options: [.resetTracking, .removeExistingAnchors])
            inRoom = true
            placement = nil
            placedOnSurface = false
            zoom = 1
            rotation = Self.upright             // stands flat on a table
            view.contentScaleFactor = 1           // drawn at 1× so the camera picture keeps up
            view.preferredFramesPerSecond = 30
            view.isPaused = false
            view.enableSetNeedsDisplay = false
            onStatus?("Move the iPad slowly to find a table or the floor, then tap to place the cube.")
        } else {
            guard inRoom else { return }
            session.pause()
            inRoom = false
            view.isPaused = true
            view.enableSetNeedsDisplay = true
            view.contentScaleFactor = Self.restingScale
            onStatus?(nil)
            view.setNeedsDisplay()
        }
    }

    /// Tap in camera mode: put the cube on the surface under the finger.
    func place(at point: CGPoint) {
        guard inRoom, let view, let frame = session.currentFrame else { return }
        let size = view.bounds.size
        guard size.width > 0, size.height > 0 else { return }
        let toImage = frame.displayTransform(for: interfaceOrientation, viewportSize: size).inverted()
        let normalized = CGPoint(x: point.x / size.width, y: point.y / size.height).applying(toImage)
        let query = frame.raycastQuery(from: normalized, allowing: .estimatedPlane, alignment: .horizontal)
        guard let hit = session.raycast(query).first else {
            onStatus?("No surface found there yet. Move the iPad a little and tap again.")
            return
        }
        let p = hit.worldTransform.columns.3
        placement = facingCamera(at: SIMD3(p.x, p.y, p.z), camera: frame.camera.transform)
        placedOnSurface = true
        onStatus?(nil)
    }

    /// Stops the camera (leaving the view).
    func stop() {
        session.pause()
        view?.isPaused = true
    }

    private var interfaceOrientation: UIInterfaceOrientation {
        view?.window?.windowScene?.effectiveGeometry.interfaceOrientation ?? .landscapeRight
    }

    /// A position turned (about the vertical) so the cube's front faces the camera.
    private func facingCamera(at position: SIMD3<Float>, camera: simd_float4x4) -> simd_float4x4 {
        let c = camera.columns.3
        let yaw = atan2(c.x - position.x, c.z - position.z)
        return Self.translation(position) * Self.rotationY(yaw)
    }

    // MARK: Matrices

    private func modelMatrix() -> simd_float4x4 {
        var m = matrix_identity_float4x4
        if inRoom, let placement {
            m = placement
            // On a surface, stand the cube on it rather than half inside it.
            if placedOnSurface { m = m * Self.translation(SIMD3(0, boxSize.y / 2 * zoom, 0)) }
        }
        return m * simd_float4x4(rotation) * Self.scale(zoom)
    }

    private func cameraMatrices(size: CGSize, frame: ARFrame?) -> (projection: simd_float4x4, view: simd_float4x4) {
        if inRoom, let frame {
            let o = interfaceOrientation
            return (frame.camera.projectionMatrix(for: o, viewportSize: size, zNear: 0.01, zFar: 50),
                    frame.camera.viewMatrix(for: o))
        }
        let aspect = Float(max(size.width, 1) / max(size.height, 1))
        return (Self.perspective(fovY: 0.75, aspect: aspect, near: 0.01, far: 20),
                Self.translation(SIMD3(0, 0, -0.75)))
    }

    static func translation(_ t: SIMD3<Float>) -> simd_float4x4 {
        var m = matrix_identity_float4x4
        m.columns.3 = SIMD4(t.x, t.y, t.z, 1)
        return m
    }

    static func scale(_ s: Float) -> simd_float4x4 {
        simd_float4x4(diagonal: SIMD4(s, s, s, 1))
    }

    static func rotationY(_ a: Float) -> simd_float4x4 {
        simd_float4x4(simd_quatf(angle: a, axis: SIMD3<Float>(0, 1, 0)))
    }

    /// Right-handed perspective for Metal (depth 0…1), looking down −z.
    static func perspective(fovY: Float, aspect: Float, near: Float, far: Float) -> simd_float4x4 {
        let y = 1 / tan(fovY * 0.5)
        let x = y / aspect
        let z = far / (near - far)
        return simd_float4x4(columns: (SIMD4(x, 0, 0, 0),
                                       SIMD4(0, y, 0, 0),
                                       SIMD4(0, 0, z, -1),
                                       SIMD4(0, 0, z * near, 0)))
    }

    // MARK: Grid lines

    /// Box coordinates (0…1) → model space (metres, centred on the box).
    private func local(_ u: SIMD3<Float>) -> SIMD3<Float> {
        (u - 0.5) * boxSize
    }

    private func rebuildLines() {
        var v: [ARLineVertex] = []
        func add(_ a: SIMD3<Float>, _ b: SIMD3<Float>, _ color: SIMD4<Float>) {
            let p = local(a), q = local(b)
            v.append(ARLineVertex(position: SIMD4(p, 1), color: color))
            v.append(ARLineVertex(position: SIMD4(q, 1), color: color))
        }
        let edge = SIMD4<Float>(0.85, 0.9, 1, 0.55)
        let gridColor = SIMD4<Float>(0.6, 1, 0.65, 0.28)
        let tickColor = SIMD4<Float>(0.85, 0.9, 1, 0.8)

        // The 12 edges of the box.
        let corners: [SIMD3<Float>] = (0..<8).map { SIMD3(Float($0 & 1), Float(($0 >> 1) & 1), Float(($0 >> 2) & 1)) }
        for i in 0..<8 {
            for bit in [1, 2, 4] where i & bit == 0 {
                add(corners[i], corners[i | bit], edge)
            }
        }
        if let axes {
            for line in axes.gridLines where line.count > 1 {
                for k in 0..<(line.count - 1) { add(line[k], line[k + 1], gridColor) }
            }
            // Short tick marks sticking out of the labelled edges.
            for axis in [axes.x, axes.y, axes.z] {
                let out = axis.outward / boxSize * (0.025 * boxSize.max())     // fixed length in metres
                for tick in axis.ticks { add(tick.position, tick.position + out, tickColor) }
            }
        }
        lineVertexCount = v.count
        lineBuffer = v.isEmpty ? nil : device.makeBuffer(bytes: v, length: v.count * MemoryLayout<ARLineVertex>.stride)
    }

    // MARK: Labels

    private func projectLabels(mvp: simd_float4x4, size: CGSize) {
        guard let onLabels else { return }
        guard settings?.showGrid == true, let axes else {
            onLabels([])
            return
        }
        var labels: [ARScreenLabel] = []
        var id = 0
        func project(_ u: SIMD3<Float>) -> CGPoint? {
            let p = local(u)
            let clip = mvp * SIMD4(p, 1)
            guard clip.w > 1e-5 else { return nil }
            let ndc = SIMD3(clip.x, clip.y, clip.z) / clip.w
            guard abs(ndc.x) <= 1.2, abs(ndc.y) <= 1.2 else { return nil }
            return CGPoint(x: CGFloat((ndc.x + 1) / 2) * size.width, y: CGFloat((1 - ndc.y) / 2) * size.height)
        }
        for axis in [axes.x, axes.y, axes.z] {
            let out = axis.outward / boxSize * (0.07 * boxSize.max())
            for tick in axis.ticks {
                if let pt = project(tick.position + out) {
                    labels.append(ARScreenLabel(id: id, text: tick.text, point: pt, isTitle: false))
                }
                id += 1
            }
            if let pt = project(axis.titlePosition + out * 2.4) {
                labels.append(ARScreenLabel(id: id, text: axis.title, point: pt, isTitle: true))
            }
            id += 1
        }
        onLabels(labels)
    }

    // MARK: Camera picture

    private func drawCamera(_ frame: ARFrame, encoder: MTLRenderCommandEncoder, size: CGSize,
                            commandBuffer: MTLCommandBuffer) {
        let buffer = frame.capturedImage
        guard let cache = textureCache, CVPixelBufferGetPlaneCount(buffer) >= 2 else { return }
        func texture(plane: Int, format: MTLPixelFormat) -> CVMetalTexture? {
            let w = CVPixelBufferGetWidthOfPlane(buffer, plane)
            let h = CVPixelBufferGetHeightOfPlane(buffer, plane)
            var out: CVMetalTexture?
            let status = CVMetalTextureCacheCreateTextureFromImage(nil, cache, buffer, nil, format, w, h, plane, &out)
            return status == kCVReturnSuccess ? out : nil
        }
        guard let y = texture(plane: 0, format: .r8Unorm), let cbcr = texture(plane: 1, format: .rg8Unorm),
              let yTexture = CVMetalTextureGetTexture(y), let cbcrTexture = CVMetalTextureGetTexture(cbcr) else { return }

        // Screen corners → camera image coordinates.
        let toCamera = frame.displayTransform(for: interfaceOrientation, viewportSize: size).inverted()
        let corners: [(SIMD2<Float>, CGPoint)] = [(SIMD2(-1, -1), CGPoint(x: 0, y: 1)), (SIMD2(1, -1), CGPoint(x: 1, y: 1)),
                                                  (SIMD2(-1, 1), CGPoint(x: 0, y: 0)), (SIMD2(1, 1), CGPoint(x: 1, y: 0))]
        var vertices: [ARCameraVertex] = corners.map { corner in
            let t = corner.1.applying(toCamera)
            return ARCameraVertex(position: corner.0, texCoord: SIMD2(Float(t.x), Float(t.y)))
        }
        encoder.setRenderPipelineState(cameraPipeline)
        encoder.setVertexBytes(&vertices, length: vertices.count * MemoryLayout<ARCameraVertex>.stride, index: 0)
        encoder.setFragmentTexture(yTexture, index: 0)
        encoder.setFragmentTexture(cbcrTexture, index: 1)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)

        let holder = TextureHolder([y, cbcr])
        commandBuffer.addCompletedHandler { _ in _ = holder }
    }
}

// MARK: - Drawing

extension ARVolumeRenderer: @preconcurrency MTKViewDelegate {
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
        if view.isPaused { view.setNeedsDisplay() }
    }

    func draw(in view: MTKView) {
        guard let settings, let pass = view.currentRenderPassDescriptor, let drawable = view.currentDrawable,
              let commandBuffer = queue.makeCommandBuffer() else { return }
        let size = view.bounds.size
        let frame = inRoom ? session.currentFrame : nil

        // First frame in camera mode: float the cube half a metre in front of the iPad.
        if inRoom, placement == nil, let frame, case .normal = frame.camera.trackingState {
            let cam = frame.camera.transform
            let forward = -SIMD3(cam.columns.2.x, cam.columns.2.y, cam.columns.2.z)
            var position = SIMD3(cam.columns.3.x, cam.columns.3.y, cam.columns.3.z) + forward * 0.5
            position.y -= 0.05
            placement = facingCamera(at: position, camera: cam)
        }

        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].clearColor = inRoom
            ? MTLClearColor(red: 0, green: 0, blue: 0, alpha: 1)
            : MTLClearColor(red: 0.02, green: 0.025, blue: 0.035, alpha: 1)
        guard let encoder = commandBuffer.makeRenderCommandEncoder(descriptor: pass) else { return }

        if let frame { drawCamera(frame, encoder: encoder, size: size, commandBuffer: commandBuffer) }

        let (projection, viewMatrix) = cameraMatrices(size: size, frame: frame)
        let showCube = !inRoom || placement != nil
        let mvp = projection * viewMatrix * modelMatrix()

        if showCube, let volumeTexture, let colormapTexture {
            var u = ARVolumeUniforms(
                inverseMVP: mvp.inverse,
                boxMin: SIMD4(-boxSize / 2, 0),
                boxSize: SIMD4(boxSize, boxSize.max()),
                params: SIMD4(Float(settings.lo), Float(settings.hi), settings.density, 0),
                dims: SIMD4(UInt32(dims.x), UInt32(dims.y), UInt32(dims.z), UInt32(dims.x + dims.y + dims.z + 4)))
            encoder.setRenderPipelineState(volumePipeline)
            encoder.setFragmentBytes(&u, length: MemoryLayout<ARVolumeUniforms>.stride, index: 0)
            encoder.setFragmentTexture(volumeTexture, index: 0)
            encoder.setFragmentTexture(colormapTexture, index: 1)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 3)
        }

        if showCube, settings.showGrid, let lineBuffer, lineVertexCount > 0 {
            var m = mvp
            encoder.setRenderPipelineState(linePipeline)
            encoder.setVertexBuffer(lineBuffer, offset: 0, index: 0)
            encoder.setVertexBytes(&m, length: MemoryLayout<simd_float4x4>.stride, index: 1)
            encoder.drawPrimitives(type: .line, vertexStart: 0, vertexCount: lineVertexCount)
        }

        encoder.endEncoding()
        commandBuffer.present(drawable)
        commandBuffer.commit()

        if showCube {
            projectLabels(mvp: mvp, size: size)
        } else {
            onLabels?([])
        }
    }
}

// MARK: - Camera tracking messages

extension ARVolumeRenderer: @preconcurrency ARSessionDelegate {
    func session(_ session: ARSession, cameraDidChangeTrackingState camera: ARCamera) {
        guard inRoom else { return }
        switch camera.trackingState {
        case .notAvailable:
            onStatus?("The camera can't track the room right now.")
        case .limited(let reason):
            switch reason {
            case .initializing: onStatus?("Starting the camera… move the iPad slowly.")
            case .excessiveMotion: onStatus?("Move the iPad more slowly.")
            case .insufficientFeatures: onStatus?("Point the camera at a table or floor with some detail.")
            case .relocalizing: onStatus?("Finding your place again…")
            @unknown default: onStatus?("Move the iPad slowly.")
            }
        case .normal:
            onStatus?(placedOnSurface ? nil : "Tap a table or the floor to place the cube.")
        }
    }

    func session(_ session: ARSession, didFailWithError error: Error) {
        // Stop here first, so the message below isn't cleared when SwiftUI turns camera mode off.
        setInRoom(false)
        onStatus?("The camera stopped: \(error.localizedDescription)")
        onRoomFailed?()
    }
}
