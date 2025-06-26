/**
 ContentView.swiftv
 -----------------
 Defines the main SwiftUI view hierarchy and rendering pipeline for the FITS Viewer app.
 - Manages file loading, header parsing, and UI state.
 - Coordinates asynchronous FITS decoding and GPU/CPU rendering updates.
 - Provides pan/zoom gestures and adjustable visualization controls.
 */
import SwiftUI
import UIKit
import Combine
import UniformTypeIdentifiers
import ImageIO
import MetalKit
import simd

/// MARK: - FITS Document Model
/// Conforms to FileDocument to allow SwiftUI DocumentGroup integration.
struct FITSDocument: FileDocument {
    static var readableContentTypes: [UTType] { [.data] }
    var data: Data

    /// Initializes a new FITSDocument with raw data, defaulting to an empty buffer.
    init(data: Data = Data()) {
        self.data = data
    }

    /// Reads document contents from the provided file wrapper and stores as raw data.
    /// - Parameters:
    ///   - configuration: SwiftUI's document read configuration.
    /// - Throws: If reading fails.
    /// Integrates with SwiftUI's document system for loading files.
    init(configuration: ReadConfiguration) throws {
        self.data = configuration.file.regularFileContents ?? Data()
    }

    /// Produces a FileWrapper for saving the document's raw data back to disk.
    /// - Parameters:
    ///   - configuration: SwiftUI's document write configuration.
    /// - Returns: FileWrapper containing the data for saving.
    /// Integrates with SwiftUI's document system for saving files.
    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        return .init(regularFileWithContents: data)
    }
}

/// MARK: - App Entry Point
/// Sets up global navigation bar styling and hosts the DocumentGroup for FITS documents.
@main
struct FITSViewerApp: App {
    /// Configures UINavigationBarAppearance for consistent theming across the app.
    init() {
        let appearance = UINavigationBarAppearance()
        appearance.configureWithOpaqueBackground()
        appearance.backgroundColor = UIColor.systemBackground
        UINavigationBar.appearance().standardAppearance = appearance
        UINavigationBar.appearance().scrollEdgeAppearance = appearance
    }
    
    /// The main scene of the application using a DocumentGroup to manage FITS files.
    var body: some Scene {
        DocumentGroup(newDocument: FITSDocument()) { file in
            ContentView(document: file.document)
        }
    }
}

/// MARK: - Scaling Modes
/// Defines intensity scaling options for FITS data visualization.
enum ScaleMode: CaseIterable {
    case linear, log, square
}

/// MARK: - Color Maps
/// Lists the available color mapping presets for rendering intensity values.
enum ColorMap: String, CaseIterable {
    case gray, hot, jet, inferno
}

/// MARK: - Metal FITS View
/// A UIViewRepresentable wrapper around MTKView for GPU-based FITS rendering
struct MetalFITSView: UIViewRepresentable {
    @Binding var floats: [Float]
    @Binding var width: Int
    @Binding var height: Int
    var bscale: Float
    var bzero: Float
    var physMin: Float
    var physMax: Float
    var clipPct: SIMD2<Float>
    var scaleModeIndex: UInt
    var colorMapIndex: UInt

    func makeUIView(context: Context) -> MTKView {
        let view = MTKView(frame: .zero, device: MTLCreateSystemDefaultDevice())
        view.framebufferOnly = false
        view.delegate = context.coordinator
        context.coordinator.setup(view: view)
        
        
        // Disables layer interpolation, raw pixels
        view.layer.magnificationFilter = .nearest
        view.layer.minificationFilter = .nearest
        
        return view
    }

    func updateUIView(_ uiView: MTKView, context: Context) {
        context.coordinator.update(
            floats: floats,
            width: width,
            height: height,
            bscale: bscale,
            bzero: bzero,
            physMin: physMin,
            physMax: physMax,
            clipPct: clipPct,
            scaleModeIndex: scaleModeIndex,
            colorMapIndex: colorMapIndex
        )
        uiView.draw()
    }

    func makeCoordinator() -> Renderer { Renderer() }

    class Renderer: NSObject, MTKViewDelegate {
        func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
            // No-op
        }
        private var device: MTLDevice!
        private var queue: MTLCommandQueue!
        private var pipeline: MTLRenderPipelineState!
        private var texture: MTLTexture?

        private var bscale: Float = 1.0
        private var bzero: Float = 0.0
        private var physMin: Float = 0.0
        private var physMax: Float = 1.0
        private var clipPct: SIMD2<Float> = [0,1]
        private var scaleModeIndex: UInt = 0
        private var colorMapIndex: UInt = 0

        func setup(view: MTKView) {
            device = view.device!
            queue = device.makeCommandQueue()
            let lib = device.makeDefaultLibrary()!
            let desc = MTLRenderPipelineDescriptor()
            desc.vertexFunction = lib.makeFunction(name: "vertex_pass")
            desc.fragmentFunction = lib.makeFunction(name: "fragment_fits")
            desc.colorAttachments[0].pixelFormat = view.colorPixelFormat
            pipeline = try! device.makeRenderPipelineState(descriptor: desc)
        }

        func update(floats: [Float], width: Int, height: Int,
                    bscale: Float, bzero: Float,
                    physMin: Float, physMax: Float,
                    clipPct: SIMD2<Float>, scaleModeIndex: UInt, colorMapIndex: UInt) {
            if texture == nil || texture!.width != width || texture!.height != height {
                let desc = MTLTextureDescriptor.texture2DDescriptor(
                    pixelFormat: .r32Float, width: width, height: height, mipmapped: false)
                texture = device.makeTexture(descriptor: desc)
            }
            let region = MTLRegionMake2D(0, 0, width, height)
            texture!.replace(region: region, mipmapLevel: 0,
                             withBytes: floats,
                             bytesPerRow: width * MemoryLayout<Float>.stride)
            self.bscale = bscale
            self.bzero = bzero
            self.physMin = physMin
            self.physMax = physMax
            self.clipPct = clipPct
            self.scaleModeIndex = scaleModeIndex
            self.colorMapIndex = colorMapIndex
        }

        func draw(in view: MTKView) {
            guard let drawable = view.currentDrawable,
                  let passDesc = view.currentRenderPassDescriptor,
                  let tex = texture else { return }
            let cmdBuf = queue.makeCommandBuffer()!
            let encoder = cmdBuf.makeRenderCommandEncoder(descriptor: passDesc)!
            encoder.setRenderPipelineState(pipeline)
            encoder.setFragmentTexture(tex, index: 0)
            encoder.setFragmentBytes(&bscale, length: MemoryLayout<Float>.stride, index: 0)
            encoder.setFragmentBytes(&bzero, length: MemoryLayout<Float>.stride, index: 1)
            encoder.setFragmentBytes(&physMin, length: MemoryLayout<Float>.stride, index: 2)
            encoder.setFragmentBytes(&physMax, length: MemoryLayout<Float>.stride, index: 3)
            encoder.setFragmentBytes(&clipPct, length: MemoryLayout<SIMD2<Float>>.stride, index: 4)
            encoder.setFragmentBytes(&scaleModeIndex, length: MemoryLayout<UInt>.stride, index: 5)
            encoder.setFragmentBytes(&colorMapIndex, length: MemoryLayout<UInt>.stride, index: 6)
            encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)
            encoder.endEncoding()
            cmdBuf.present(drawable)
            cmdBuf.commit()
        }
    }
}

/// MARK: - ContentView
/// Core SwiftUI view that:
/// - Hosts the NavigationView and toolbar.
/// - Manages FITS loading phases and UI states.
/// - Coordinates rendering triggers and parameter adjustments.
struct ContentView: View {
    /// The underlying FITSDocument provided by the DocumentGroup.
    private let document: FITSDocument

    /// Initializes the ContentView with a reference to the loaded FITS document.
    init(document: FITSDocument) {
        self.document = document
    }

    /// MARK: - Loading Phases
    /// Represents the various states of file loading and rendering.
    enum Phase: Equatable { case selectFile, loading, viewing, error(String) }

    // ==========================================================================
    // MARK: - View State Properties
    // - phase: current load/view/error state
    // - hasLoaded: prevents re-loading on view appearance
    // - fitsFloats, fitsWidth/Height: raw pixel buffer and dimensions
    // - bscale/bzero: FITS scaling parameters
    // - uiImage: generated thumbnail for display
    // - scaleMode, clipMinPct, clipMaxPct, colorMap: visualization parameters
    // - showPicker/showSidebar/showHeader: UI toggles for controls
    // - zoomScale/lastZoomValue/panOffset/lastPanOffset: gesture state
    // - headerDict: parsed FITS header key-value pairs
    // ==========================================================================
    @State private var phase: Phase = .loading
    @State private var hasLoaded = false

    // FITS data
    @State private var fitsFloats: [Float] = []
    @State private var fitsWidth = 0
    @State private var fitsHeight = 0
    @State private var bscale: Double = 1.0
    @State private var bzero: Double = 0.0

    // Rendering state
    @State private var uiImage: UIImage? = nil
    @State private var scaleMode: ScaleMode = .linear
    @State private var clipMinPct: Double = 0
    @State private var clipMaxPct: Double = 100
    @State private var colorMap: ColorMap = .gray
    @State private var showPicker = true
    @State private var showSidebar = true

    @State private var zoomScale: CGFloat = 1.0
    @State private var lastZoomValue: CGFloat = 1.0
    @State private var panOffset: CGSize = .zero
    @State private var lastPanOffset: CGSize = .zero

    @State private var showHeader = false
    @State private var headerDict: [String: String] = [:]

    // For Metal rendering: store computed low/high values
    @State private var lowVal: Float = 0
    @State private var highVal: Float = 1

    /// Subject used to debounce and throttle rendering requests on parameter changes.
    private let renderTrigger = PassthroughSubject<Void, Never>()
    /// Holds the cancellable for the Combine pipeline that throttles render updates.
    @State private var renderCancellable: AnyCancellable?

    // ==========================================================================
    // MARK: - View Body
    // Builds the main NavigationView and dynamic content based on `phase`.
    // ==========================================================================
    var body: some View {
        NavigationView {
            Group {
                switch phase {
                case .selectFile: selectScreen
                case .loading: loadingScreen
                case .viewing: viewerScreen
                case .error(let msg): errorScreen(msg)
                }
            }
            .navigationTitle("FITS Viewer")
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { showHeader.toggle() }) {
                        Image(systemName: "text.alignleft")
                    }
                    .disabled(headerDict.isEmpty)
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: {
                        withAnimation(.easeInOut) {
                            showSidebar.toggle()
                        }
                    }) {
                        Image(systemName: "v.square.fill")
                    }
                }
            }
        }
        .sheet(isPresented: $showHeader) {
            NavigationView {
                List(headerDict.sorted(by: { $0.key < $1.key }), id: \.key) { key, value in
                    HStack {
                        Text(key).bold()
                        Spacer()
                        Text(value)
                    }
                }
                .navigationTitle("FITS Header")
                .toolbar {
                    ToolbarItem(placement: .cancellationAction) {
                        Button("Close") { showHeader = false }
                    }
                }
            }
        }
        .onAppear {
//            // Throttle live updates to avoid stutter on large files
//            renderCancellable = renderTrigger
//                // at most 20 updates per second
//                .throttle(for: .milliseconds(50),
//                          scheduler: DispatchQueue.global(),
//                          latest: true)
//                .sink { _ in
//                    // generate the new UIImage off the main thread
//                    let raw = renderToUIImage()
//                    let maxDim = max(UIScreen.main.bounds.width, UIScreen.main.bounds.height)
//                    let thumb = downsample(raw, maxDimension: maxDim)
//                    // push it back to the UI
//                    DispatchQueue.main.async { uiImage = thumb }
//                }
        }
        .task {
            guard !hasLoaded else { return }
            hasLoaded = true
            // write document.data to temp URL
            let tempURL = FileManager.default.temporaryDirectory
                .appendingPathComponent("temp.fits")
            do {
                try document.data.write(to: tempURL)
                loadFitsAsync(tempURL)
            } catch {
                phase = .error("Failed to load document data: \(error)")
            }
        }
        .onChange(of: phase) { newPhase in
            if case .selectFile = newPhase {
                // Clear loaded image and data when returning to picker
                fitsFloats.removeAll(keepingCapacity: false)
                fitsWidth = 0
                fitsHeight = 0
                uiImage = nil
            }
        }
    }

    /// Screen shown when no file is selected, prompting the user to open a FITS file.
    private var selectScreen: some View {
        VStack(spacing: 20) {
            Text("Select a FITS file to view")
            Button("Open FITS File") { showPicker = true }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    /// Simple progress indicator shown while FITS data is being loaded.
    private var loadingScreen: some View {
        ProgressView("Loading FITS…")
            .progressViewStyle(CircularProgressViewStyle())
            .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
    
    /// Main viewer UI composed of:
    /// - A GeometryReader-wrapped MetalFITSView for GPU rendering.
    /// - A sidebar Form containing visualization controls.
    private var viewerScreen: some View {
        // Precompute physical range based on scale mode
        let rawMin = fitsFloats.min() ?? 0
        let rawMax = fitsFloats.max() ?? 1
        let baseMin = Float(bzero) + Float(bscale) * rawMin
        let baseMax = Float(bzero) + Float(bscale) * rawMax

        let physMinValue: Float
        let physMaxValue: Float
        switch scaleMode {
        case .square:
            physMinValue = baseMin * baseMin
            physMaxValue = baseMax * baseMax
        case .log:
            let shift = min(0, baseMin) * -1
            physMinValue = log10(baseMin + shift + 1)
            physMaxValue = log10(baseMax + shift + 1)
        default:
            physMinValue = baseMin
            physMaxValue = baseMax
        }

        let clipPctValue = SIMD2<Float>(Float(clipMinPct / 100.0), Float(clipMaxPct / 100.0))
        let scaleIndex = UInt([ScaleMode.linear, .square, .log].firstIndex(of: scaleMode) ?? 0)
        let mapIndex = UInt(ColorMap.allCases.firstIndex(of: colorMap) ?? 0)

        return HStack(spacing: 0) {
            // ——— The Metal GPU image viewer ———
            GeometryReader { geo in
                MetalFITSView(
                    floats: $fitsFloats,
                    width: $fitsWidth,
                    height: $fitsHeight,
                    bscale: Float(bscale),
                    bzero: Float(bzero),
                    physMin: physMinValue,
                    physMax: physMaxValue,
                    clipPct: clipPctValue,
                    scaleModeIndex: scaleIndex,
                    colorMapIndex: mapIndex
                )
                // Maintain the image's aspect ratio
                .aspectRatio(
                    CGFloat(fitsWidth) / CGFloat(fitsHeight),
                    contentMode: .fit
                )
                // Apply user-controlled scale and translation
                .scaleEffect(zoomScale)
                .offset(panOffset)
                // Zoom gesture
                .gesture(
                    MagnificationGesture()
                        .onChanged { value in
                            let delta = value / lastZoomValue
                            lastZoomValue = value
                            zoomScale *= delta
                        }
                        .onEnded { _ in lastZoomValue = 1.0 }
                )
                // Pan gesture
                .simultaneousGesture(
                    DragGesture()
                        .onChanged { value in
                            let translation = value.translation
                            let delta = CGSize(
                                width: translation.width - lastPanOffset.width,
                                height: translation.height - lastPanOffset.height
                            )
                            lastPanOffset = translation
                            panOffset = CGSize(
                                width: panOffset.width + delta.width,
                                height: panOffset.height + delta.height
                            )
                        }
                        .onEnded { _ in lastPanOffset = .zero }
                )
                .frame(width: geo.size.width, height: geo.size.height)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .layoutPriority(1)
            .background(Color(UIColor.lightGray))

            // ——— The sidebar of controls ———
            if showSidebar {
                Form {
                    Section(header: Text("Scale Mode")) {
                        Picker("Scale", selection: $scaleMode) {
                            Text("Linear").tag(ScaleMode.linear)
                            Text("Log").tag(ScaleMode.log)
                            Text("Square").tag(ScaleMode.square)
                        }
                        .pickerStyle(.segmented)
                    }

                    Section(header: Text("Clip Percentiles")) {
                        // Quick presets
                        ScrollView(.horizontal, showsIndicators: false) {
                            HStack(spacing: 8) {
                                ForEach([90.0, 95.0, 99.0, 99.5, 99.9, 99.95, 99.99, 100.0], id: \.self) { p in
                                    Button("\(p, specifier: p.truncatingRemainder(dividingBy: 1)==0 ? "%.0f" : "%.2f")%") {
                                        clipMaxPct = p
//                                        renderTrigger.send()
                                    }
                                    .buttonStyle(.bordered)
                                }
                            }
                            .padding(.vertical, 4)
                        }
                        // Min slider
                        HStack {
                            Text("Min: \(Int(clipMinPct))%")
                            Slider(value: $clipMinPct, in: 0...clipMaxPct)
                                .onChange(of: clipMinPct) { _ in /*renderTrigger.send() */}
                        }
                        // Max slider
                        HStack {
                            Text("Max: \(clipMaxPct, specifier: clipMaxPct.truncatingRemainder(dividingBy: 1)==0 ? "%.0f" : "%.2f")%")
                            Slider(value: $clipMaxPct, in: clipMinPct...100)
                                .onChange(of: clipMaxPct) { _ in /*renderTrigger.send()*/ }
                        }
                    }

                    Section(header: Text("Color Map")) {
                        Picker("Color Map", selection: $colorMap) {
                            ForEach(ColorMap.allCases, id: \.self) { cm in
                                Text(cm.rawValue.capitalized)
                                    .tag(cm)
                            }
                        }
                        .pickerStyle(.segmented)
                    }
                }
                .padding(.vertical)
                .frame(width: 350)
                .background(
                    RoundedRectangle(cornerRadius: 12)
                        .fill(Color(UIColor.secondarySystemBackground))
                )
                .padding(.vertical, 8)
            }
        }
        .background(Color(UIColor.lightGray))
        .onChange(of: scaleMode)   { _,_ in /*renderTrigger.send()*/ }
        .onChange(of: colorMap)    { _,_ in /*renderTrigger.send()*/ }
        .animation(.easeInOut, value: showSidebar)
    }
    
    /// Displays any loading or decoding errors with a retry button.
    private func errorScreen(_ msg: String) -> some View {
        VStack(spacing: 20) {
            Text("Error: \(msg)")
                .foregroundColor(.red)
            Button("Try Again") { phase = .selectFile }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    /// MARK: - Asynchronous FITS Loading
    /// Loads and parses the FITS file off the main thread, then triggers rendering.
    private func loadFitsAsync(_ url: URL) {
        // Clearing previous data to free memory
        fitsFloats.removeAll(keepingCapacity: false)
        uiImage = nil
        phase = .loading
        DispatchQueue.global(qos: .userInitiated).async {
            // Request permission to access the file if needed
            let ok = url.startAccessingSecurityScopedResource()
            defer { if ok { url.stopAccessingSecurityScopedResource() } }
            guard ok else {
                // If permission denied, show error on main thread
                DispatchQueue.main.async { phase = .error("Permission denied") }
                return
            }
            do {
                // Try loading the FITS file and parse contents
                try loadFITS(from: url)
                clipMinPct = 0; clipMaxPct = 100
//                renderTrigger.send()
                // Switch to viewing phase on main thread
                DispatchQueue.main.async { phase = .viewing }
            } catch {
                // Show error if loading or parsing fails
                DispatchQueue.main.async { phase = .error(error.localizedDescription) }
            }
        }
    }

    /// MARK: - CPU Rendering Pipeline
    /// Converts the raw float buffer into a UIImage:
    /// 1. Applies BSCALE/BZERO and scale mode transforms.
    /// 2. Computes percentile-based clipping.
    /// 3. Maps float values to RGBA bytes using the selected color map.
    /// 4. Constructs a CGImage and wraps it in UIImage.
//    private func renderToUIImage() -> UIImage {
//        // Apply bzero/bscale to get physical values
//        let phys = fitsFloats.map { Float(bzero) + Float(bscale) * $0 }
//        // Sort values for percentile clipping
//        let sorted = phys.sorted()
//        let count = sorted.count
//        // Compute integer indices based on rounded percentiles
//        let idxScale = Double(count - 1) / 100.0
//        let lowIndex  = max(0, min(count-1, Int(round(clipMinPct * idxScale))))
//        let highIndex = max(0, min(count-1, Int(round(clipMaxPct * idxScale))))
//        let lv    = sorted[lowIndex]
//        let hv   = sorted[highIndex]
//        // Save for Metal rendering
//        DispatchQueue.main.async {
//            self.lowVal = lv
//            self.highVal = hv
//        }
//        let rng = (hv - lv) != 0 ? (hv - lv) : 1
//        // Map to RGBA pixels using color map
//        var pixels = [UInt8](repeating: 0, count: fitsWidth * fitsHeight * 4)
//        for i in 0..<count {
//            var t = (phys[i] - lv) / rng
//            if !t.isFinite { t = 0 }
//            t = min(max(t, 0), 1)
//            let (r,g,b): (Float,Float,Float)
//            switch colorMap {
//            case .gray: (r,g,b) = (t,t,t)
//            case .hot:
//                if t < 0.33 { (r,g,b) = (t*3,0,0) }
//                else if t < 0.66 { (r,g,b) = (1,(t-0.33)*3,0) }
//                else { (r,g,b) = (1,1,(t-0.66)*3) }
//            case .jet:
//                func f(_ x: Float) -> Float { min(max(1.5-abs(4*t-x),0),1) }
//                (r,g,b) = (f(3),f(2),f(1))
//            }
//            pixels[i*4+0] = UInt8(clamping: Int(min(max(r,0),1)*255))
//            pixels[i*4+1] = UInt8(clamping: Int(min(max(g,0),1)*255))
//            pixels[i*4+2] = UInt8(clamping: Int(min(max(b,0),1)*255))
//            pixels[i*4+3] = 255
//        }
//        // Create CGImage and wrap in UIImage
//        let provider = CGDataProvider(data: Data(pixels) as CFData)!
//        let cgImage = CGImage(
//            width: fitsWidth, height: fitsHeight,
//            bitsPerComponent: 8, bitsPerPixel: 32,
//            bytesPerRow: fitsWidth * 4,
//            space: CGColorSpaceCreateDeviceRGB(),
//            bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.last.rawValue),
//            provider: provider, decode: nil,
//            shouldInterpolate: false, intent: .defaultIntent
//        )!
//        return UIImage(cgImage: cgImage)
//    }

    /// MARK: - FITS File Parsing
    /// Low-level parser that:
    /// 1. Reads header blocks of 2880 bytes until 'END' card.
    /// 2. Parses key/value cards into a dictionary.
    /// 3. Calculates data block size from NAXIS and BITPIX.
    /// 4. Decodes the first 2D image HDU into floats.
    /// 5. Skips non-image HDUs safely.
    private func loadFITS(from url: URL) throws {
        let data = try Data(contentsOf: url, options: .mappedIfSafe)
        var offset = 0

        // Loop through all Header/Data Units (HDUs) in the file
        while offset < data.count {
            let hduStartOffset = offset

            // --- 1. Read the header for the current HDU ---
            // A FITS header is composed of one or more 2880-byte records
            var headerData = Data()
            var headerString = ""
            var endCardFound = false
            
            // Header reading loop: Read 2880-byte blocks until 'END' card is found
            while offset < data.count {
                let blockEnd = min(offset + 2880, data.count)
                guard blockEnd > offset else { break }

                let currentBlockData = data[offset..<blockEnd]
                headerData.append(currentBlockData)
                offset = blockEnd

                // A valid FITS header ends with an 'END' card. We check the entire
                // header string we've built so far to see if it's present.
                if let cumulativeString = String(data: headerData, encoding: .ascii) {
                    if cumulativeString.contains("END" + String(repeating: " ", count: 77)) {
                        headerString = cumulativeString
                        endCardFound = true
                        break
                    }
                } else {
                    // If the block isn't valid ASCII, it's not a header.
                    throw NSError(domain: "FITS", code: -6, userInfo: [NSLocalizedDescriptionKey: "Corrupt data found while reading header."])
                }
            }

            // If we exit the loop without finding an END card, the file is likely truncated or corrupt.
            guard endCardFound else { break }

            let dataStartOffset = offset

            // --- 2. Parse the header string into a dictionary ---
            // Header parsing into cards
            let cards = headerString.chunked(into: 80)
            var currentHeaderDict = [String: String]()
            for card in cards {
                if card.starts(with: "END") { break }
                let key = String(card.prefix(8)).trimmingCharacters(in: .whitespaces)
                guard !key.isEmpty, card.count > 10, card[card.index(card.startIndex, offsetBy: 8)...].starts(with: "= ") else { continue }
                guard let eqIdx = card.firstIndex(of: "=") else { continue }

                let valueAndComment = card[card.index(after: eqIdx)...]
                let rawValue = valueAndComment.split(separator: "/")[0]
                    .trimmingCharacters(in: .whitespaces)
                    .trimmingCharacters(in: CharacterSet(charactersIn: "' ")) // Removes surrounding spaces and quotes
                currentHeaderDict[key] = String(rawValue)
            }

            // --- 3. Calculate the size of this HDU's data block ---
            // Data-size calculation
            let naxis = Int(currentHeaderDict["NAXIS"] ?? "0") ?? 0
            let bitpix = Int(currentHeaderDict["BITPIX"] ?? "0") ?? 0
            var dataSize = 0

            if naxis > 0 && bitpix != 0 {
                var elements = 1
                for i in 1...naxis {
                    // A valid FITS file with NAXIS=N MUST have NAXISi keywords for i=1..N.
                    // If one is missing, we cannot calculate the size, so the header is corrupt.
                    guard let naxis_i_str = currentHeaderDict["NAXIS\(i)"], let naxis_i = Int(naxis_i_str) else {
                        throw NSError(domain: "FITS", code: -5, userInfo: [NSLocalizedDescriptionKey: "Corrupt HDU: NAXIS=\(naxis) but NAXIS\(i) is missing or invalid."])
                    }
                    elements *= naxis_i
                }
                dataSize = (abs(bitpix) / 8) * elements
            }
            
            // --- 4. Decide whether to load this HDU as an image or skip it ---
            // Image HDU detection and float conversion
            let w = Int(currentHeaderDict["NAXIS1"] ?? "0") ?? 0
            let h = Int(currentHeaderDict["NAXIS2"] ?? "0") ?? 0
            
            // We will load the *first* HDU we find that is a 2D image (or a cube).
            if naxis >= 2 && w > 0 && h > 0 && bitpix != 0 {
                // This is a loadable image. Set properties and decode the pixel data.
                DispatchQueue.main.async {
                    self.headerDict = currentHeaderDict
                }
                bscale = Double(currentHeaderDict["BSCALE"] ?? "1.0") ?? 1.0
                bzero = Double(currentHeaderDict["BZERO"] ?? "0.0") ?? 0.0

                guard dataStartOffset + dataSize <= data.count else {
                    throw NSError(domain: "FITS", code: -2, userInfo: [NSLocalizedDescriptionKey: "Image data incomplete."])
                }
                
                // For data cubes (NAXIS > 2), we'll only load the first 2D slice.
                let pixelsInSlice = w * h
                let bytesPerPixel = abs(bitpix) / 8
                let sliceDataSize = pixelsInSlice * bytesPerPixel
                
                let slice = data[dataStartOffset..<(dataStartOffset + sliceDataSize)]
                fitsWidth = w
                fitsHeight = h
                
                var floats = [Float](repeating: 0, count: pixelsInSlice)
                
                slice.withUnsafeBytes { buf in
                    guard let base = buf.baseAddress else { return }
                    DispatchQueue.concurrentPerform(iterations: pixelsInSlice) { i in
                        let ptr = base.advanced(by: i * bytesPerPixel)
                        let rv: Float
                        switch bitpix {
                        case 8: rv = Float(ptr.load(as: UInt8.self))
                        case 16: rv = Float(Int16(bitPattern: UInt16(bigEndian: ptr.load(as: UInt16.self))))
                        case 32: rv = Float(Int32(bitPattern: UInt32(bigEndian: ptr.load(as: UInt32.self))))
                        case -32: rv = Float(bitPattern: UInt32(bigEndian: ptr.load(as: UInt32.self)))
                        case -64: rv = Float(Double(bitPattern: UInt64(bigEndian: ptr.load(as: UInt64.self))))
                        case 64: rv = Float(Int64(bitPattern: UInt64(bigEndian: ptr.load(as: UInt64.self))))
                        default: rv = 0
                        }
                        floats[i] = rv
                    }
                }
                self.fitsFloats = floats
                return // Success! We've loaded the image and can exit.
            }

            // --- 5. If we didn't load this HDU, skip its data block to get to the next HDU ---
            // Block skipping logic
            let paddedDataSize = (dataSize + 2879) / 2880 * 2880
            offset = dataStartOffset + paddedDataSize

            // Safety check to prevent an infinite loop on a corrupt file
            if offset <= hduStartOffset {
                throw NSError(domain: "FITS", code: -3, userInfo: [NSLocalizedDescriptionKey: "Failed to advance in FITS file, file may be corrupt."])
            }
        }

        // If the while loop finishes without returning, no loadable image was found.
        throw NSError(domain: "FITS", code: -1, userInfo: [NSLocalizedDescriptionKey: "No 2D image HDU found in the file."])
    }
}

/// MARK: - String Utilities
/// Splits a string into fixed-size chunks, used for parsing FITS header cards.
extension String {
    func chunked(into size: Int) -> [String] {
        var out: [String] = []
        var idx = startIndex
        while idx < endIndex {
            let endIdx = index(idx, offsetBy: size, limitedBy: endIndex) ?? endIndex
            out.append(String(self[idx..<endIdx]))
            idx = endIdx
        }
        return out
    }
}

/// MARK: - Downsampling Helper
/// Creates a thumbnail by leveraging CGImageSource to limit memory usage
/// when displaying large images.
private func downsample(_ image: UIImage, maxDimension: CGFloat) -> UIImage {
    guard let data = image.pngData() else { return image }
    let options: [CFString: Any] = [
        kCGImageSourceShouldCache: false,
        kCGImageSourceCreateThumbnailFromImageAlways: true,
        kCGImageSourceThumbnailMaxPixelSize: maxDimension
    ]
    let source = CGImageSourceCreateWithData(data as CFData, nil)!
    let cgThumb = CGImageSourceCreateThumbnailAtIndex(source, 0, options as CFDictionary)!
    return UIImage(cgImage: cgThumb)
}
