/**
 ContentView.swiftv
 -----------------
 Defines the main SwiftUI view hierarchy and rendering pipeline for the FITS Viewer app.
 - Manages file loading, header parsing, and UI state.
 - Coordinates asynchronous FITS decoding and GPU/CPU rendering updates.
 - Provides pan/zoom gestures and adjustable visualization controls.
 */
import SwiftUI
import UniformTypeIdentifiers
import MetalKit

/// MARK: - FITS Document Model
/// Conforms to FileDocument to allow SwiftUI DocumentGroup integration.
///
extension UTType {
  /// Dynamically wraps the “fits” extension in a UTType.
  static let fitsFile = UTType(filenameExtension: "fits")!
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

    /// The main scene of the application using a WindowGroup and manual document picker.
    var body: some Scene {
        WindowGroup {
            ContentView()
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
    @Binding var zoomScale: CGFloat

    func makeUIView(context: Context) -> MTKView {
        let view = MTKView(frame: .zero, device: MTLCreateSystemDefaultDevice())
        view.framebufferOnly = false
        view.delegate = context.coordinator
        context.coordinator.setup(view: view)
        // Disables layer interpolation, raw pixels
        view.layer.magnificationFilter = .nearest
        view.layer.minificationFilter = .nearest
        view.enableSetNeedsDisplay = true
        view.isPaused = true
        return view
    }

    func updateUIView(_ uiView: MTKView, context: Context) {
        uiView.drawableSize = CGSize(
            width: CGFloat(width) * zoomScale,
            height: CGFloat(height) * zoomScale
        )
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
            // Prevent creating textures with invalid dimensions
            guard width > 0 && height > 0 else { return }
            if texture == nil || texture!.width != width || texture!.height != height {
                let desc = MTLTextureDescriptor.texture2DDescriptor(
                    pixelFormat: .r32Float, width: width, height: height, mipmapped: false)
                desc.usage = [.shaderRead]
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
/// - Hosts the NavigationSplitView and toolbar.
/// - Manages FITS loading phases and UI states.
/// - Coordinates rendering triggers and parameter adjustments.
struct ContentView: View {

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

    // Manual document picker state
    @State private var showPicker: Bool = false
    @State private var documentData: Data = Data()

    // FITS data
    @State private var fitsFloats: [Float] = []
    @State private var fitsWidth = 0
    @State private var fitsHeight = 0
    @State private var bscale: Double = 1.0
    @State private var bzero: Double = 0.0

    // Rendering state
    @State private var scaleMode: ScaleMode = .linear
    @State private var clipMinPct: Double = 0
    @State private var clipMaxPct: Double = 100
    @State private var colorMap: ColorMap = .gray
    @State private var showSidebar = true

    @State private var zoomScale: CGFloat = 1.0
    @State private var panOffset: CGSize = .zero
    // Live scale factor during an active pinch gesture
    @GestureState private var gestureScale: CGFloat = 1.0
    // Live translation during an active drag gesture
    @GestureState private var gesturePan: CGSize = .zero

    @State private var showHeader = false
    @State private var headerDict: [String: String] = [:]


    // The selected file name for toolbar title
    @State private var fileName: String = ""



    // ==========================================================================
    // MARK: - View Body
    // Builds the main NavigationView and dynamic content based on `phase`.
    // ==========================================================================
    var body: some View {
        NavigationSplitView {
            // Sidebar
            VStack(alignment: .leading, spacing: 16) {
                Button("Open File") { showPicker = true }
                Button("Settings") { }
                Button("Contours") { }
                Spacer()
            }
            .padding()
        } detail: {
            Group {
                switch phase {
                case .selectFile: selectScreen
                case .loading: loadingScreen
                case .viewing: viewerScreen
                case .error(let msg): errorScreen(msg)
                }
            }
            .navigationTitle(fileName.isEmpty ? "FITS Viewer" : fileName)
            .navigationBarTitleDisplayMode(.inline)
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
        }
        .onAppear {
            showPicker = true
        }
        .sheet(isPresented: $showPicker) {
            DocumentPicker { url in
                guard url.startAccessingSecurityScopedResource() else {
                    phase = .error("Permission denied to access file")
                    showPicker = false
                    return
                }
                defer { url.stopAccessingSecurityScopedResource() }
                do {
                    documentData = try Data(contentsOf: url)
                    fileName = url.lastPathComponent
                    loadFitsAsync(data: documentData)
                    phase = .viewing
                } catch {
                    phase = .error(error.localizedDescription)
                }
                showPicker = false
            }
        }
        .onChange(of: phase) { newPhase in
            if case .selectFile = newPhase {
                // Clear loaded image and data when returning to picker
                fitsFloats.removeAll(keepingCapacity: false)
                fitsWidth = 0
                fitsHeight = 0
            }
        }
    }


    /// Screen shown when no file is selected, prompting the user to open a FITS file.
    private var selectScreen: some View {
        VStack(spacing: 20) {
            Text("Select a FITS file to view")
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
                    colorMapIndex: mapIndex,
                    zoomScale: $zoomScale
                )
                .aspectRatio(CGFloat(fitsWidth) / CGFloat(fitsHeight), contentMode: .fit)
                .scaleEffect(zoomScale * gestureScale)
                // Combine cumulative and active pan
                .offset(
                    x: panOffset.width + gesturePan.width,
                    y: panOffset.height + gesturePan.height
                )
                .gesture(
                    DragGesture()
                        .updating($gesturePan) { value, state, _ in
                            state = value.translation
                        }
                        .onEnded { value in
                            panOffset.width += value.translation.width
                            panOffset.height += value.translation.height
                        }
                )
                .simultaneousGesture(
                    MagnificationGesture()
                        .updating($gestureScale) { value, state, _ in
                            state = value
                        }
                        .onEnded { finalValue in
                            zoomScale *= finalValue
                        }
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
                        }
                        // Max slider
                        HStack {
                            Text("Max: \(clipMaxPct, specifier: clipMaxPct.truncatingRemainder(dividingBy: 1)==0 ? "%.0f" : "%.2f")%")
                            Slider(value: $clipMaxPct, in: clipMinPct...100)
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
    private func loadFitsAsync(data: Data) {
        fitsFloats.removeAll(keepingCapacity: false)
        phase = .loading
        DispatchQueue.global(qos: .userInitiated).async {
            do {
                try loadFITS(with: data)
                clipMinPct = 0; clipMaxPct = 100
                DispatchQueue.main.async { phase = .viewing }
            } catch {
                DispatchQueue.main.async { phase = .error(error.localizedDescription) }
            }
        }
    }
    
    /// MARK: - FITS File Parsing
    /// Low-level parser that:
    /// 1. Reads header blocks of 2880 bytes until 'END' card.
    /// 2. Parses key/value cards into a dictionary.
    /// 3. Calculates data block size from NAXIS and BITPIX.
    /// 4. Decodes the first 2D image HDU into floats.
    /// 5. Skips non-image HDUs safely.
    private func loadFITS(with data: Data) throws {
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


// MARK: - DocumentPicker for manual file selection
struct DocumentPicker: UIViewControllerRepresentable {
    var onPick: (URL) -> Void
    func makeCoordinator() -> Coordinator { Coordinator(parent: self) }
    func makeUIViewController(context: Context) -> UIDocumentPickerViewController {
        let picker = UIDocumentPickerViewController(forOpeningContentTypes: [UTType.fitsFile])
        picker.delegate = context.coordinator
        return picker
    }
    func updateUIViewController(_ uiViewController: UIDocumentPickerViewController, context: Context) {}
    class Coordinator: NSObject, UIDocumentPickerDelegate {
        let parent: DocumentPicker
        init(parent: DocumentPicker) { self.parent = parent }
        func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentsAt urls: [URL]) {
            guard let url = urls.first else { return }
            parent.onPick(url)
        }
    }
}
