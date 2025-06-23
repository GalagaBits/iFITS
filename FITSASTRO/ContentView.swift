import SwiftUI
import UIKit
import Combine
import UniformTypeIdentifiers
import ImageIO

struct FITSDocument: FileDocument {
    static var readableContentTypes: [UTType] { [.data] }
    var data: Data

    init(data: Data = Data()) {
        self.data = data
    }

    init(configuration: ReadConfiguration) throws {
        self.data = configuration.file.regularFileContents ?? Data()
    }

    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        return .init(regularFileWithContents: data)
    }
}

@main
struct FITSViewerApp: App {
    init() {
        let appearance = UINavigationBarAppearance()
        appearance.configureWithOpaqueBackground()
        appearance.backgroundColor = UIColor.systemBackground
        UINavigationBar.appearance().standardAppearance = appearance
        UINavigationBar.appearance().scrollEdgeAppearance = appearance
    }
    
    var body: some Scene {
        DocumentGroup(newDocument: FITSDocument()) { file in
            ContentView(document: file.document)
        }
    }
}

enum ScaleMode: CaseIterable {
    case linear, log, square
}

enum ColorMap: String, CaseIterable {
    case gray, hot, jet
}

struct ContentView: View {
    private let document: FITSDocument

    init(document: FITSDocument) {
        self.document = document
    }

    enum Phase: Equatable { case selectFile, loading, viewing, error(String) }
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

    private let renderTrigger = PassthroughSubject<Void, Never>()
    @State private var renderCancellable: AnyCancellable?

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
            // Throttle live updates to avoid stutter on large files
            renderCancellable = renderTrigger
                // at most 20 updates per second
                .throttle(for: .milliseconds(50),
                          scheduler: DispatchQueue.global(),
                          latest: true)
                .sink { _ in
                    // generate the new UIImage off the main thread
                    let raw = renderToUIImage()
                    let maxDim = max(UIScreen.main.bounds.width, UIScreen.main.bounds.height)
                    let thumb = downsample(raw, maxDimension: maxDim)
                    // push it back to the UI
                    DispatchQueue.main.async { uiImage = thumb }
                }
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

    private var selectScreen: some View {
        VStack(spacing: 20) {
            Text("Select a FITS file to view")
            Button("Open FITS File") { showPicker = true }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private var loadingScreen: some View {
        ProgressView("Loading FITS…")
            .progressViewStyle(CircularProgressViewStyle())
            .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
    
    private var viewerScreen: some View {
        HStack(spacing: 0) {
            // ——— The image viewer ———
            GeometryReader { geo in
                if let img = uiImage {
                    Image(uiImage: img)
                        .resizable()
                        .interpolation(.none)
                        .scaledToFit()
                        .scaleEffect(x: zoomScale, y: -zoomScale)
                        .gesture(
                            MagnificationGesture()
                                .onChanged { value in
                                    let delta = value / lastZoomValue
                                    lastZoomValue = value
                                    zoomScale *= delta
                                }
                                .onEnded { _ in lastZoomValue = 1.0 }
                        )
                        .offset(panOffset)
                        .frame(width: geo.size.width, height: geo.size.height)
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
                } else {
                    Text("Rendering…")
                        .frame(width: geo.size.width, height: geo.size.height)
                }
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
                                        renderTrigger.send()
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
                                .onChange(of: clipMinPct) { _ in renderTrigger.send() }
                        }
                        // Max slider
                        HStack {
                            Text("Max: \(clipMaxPct, specifier: clipMaxPct.truncatingRemainder(dividingBy: 1)==0 ? "%.0f" : "%.2f")%")
                            Slider(value: $clipMaxPct, in: clipMinPct...100)
                                .onChange(of: clipMaxPct) { _ in renderTrigger.send() }
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
        .onChange(of: scaleMode)   { _,_ in renderTrigger.send() }
        .onChange(of: colorMap)    { _,_ in renderTrigger.send() }
        .animation(.easeInOut, value: showSidebar)
    }
    
    private func errorScreen(_ msg: String) -> some View {
        VStack(spacing: 20) {
            Text("Error: \(msg)")
                .foregroundColor(.red)
            Button("Try Again") { phase = .selectFile }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func loadFitsAsync(_ url: URL) {
        // Clearing previous data to free memory
        fitsFloats.removeAll(keepingCapacity: false)
        uiImage = nil
        phase = .loading
        DispatchQueue.global(qos: .userInitiated).async {
            let ok = url.startAccessingSecurityScopedResource()
            defer { if ok { url.stopAccessingSecurityScopedResource() } }
            guard ok else {
                DispatchQueue.main.async { phase = .error("Permission denied") }
                return
            }
            do {
                try loadFITS(from: url)
                clipMinPct = 0; clipMaxPct = 100
                renderTrigger.send()
                DispatchQueue.main.async { phase = .viewing }
            } catch {
                DispatchQueue.main.async { phase = .error(error.localizedDescription) }
            }
        }
    }

    private func renderToUIImage() -> UIImage {
        var phys = fitsFloats.map { Float(bzero) + Float(bscale) * $0 }
        if scaleMode == .square {
            phys = phys.map { $0 * $0 }
        }
        if scaleMode == .log {
            let minv = phys.min() ?? 0
            let shift = min(0, minv) * -1
            phys = phys.map { log10($0 + shift + 1) }
        }
        let sorted = phys.sorted()
        let count = sorted.count
        // Compute integer indices based on rounded percentiles
        let idxScale = Double(count - 1) / 100.0
        let lowIndex  = max(0, min(count-1, Int(round(clipMinPct * idxScale))))
        let highIndex = max(0, min(count-1, Int(round(clipMaxPct * idxScale))))
        let lowVal    = sorted[lowIndex]
        let highVal   = sorted[highIndex]
        let rng = (highVal - lowVal) != 0 ? (highVal - lowVal) : 1
        var pixels = [UInt8](repeating: 0, count: fitsWidth * fitsHeight * 4)
        for i in 0..<count {
            var t = (phys[i] - lowVal) / rng
            if !t.isFinite { t = 0 }
            t = min(max(t, 0), 1)
            let (r,g,b): (Float,Float,Float)
            switch colorMap {
            case .gray: (r,g,b) = (t,t,t)
            case .hot:
                if t < 0.33 { (r,g,b) = (t*3,0,0) }
                else if t < 0.66 { (r,g,b) = (1,(t-0.33)*3,0) }
                else { (r,g,b) = (1,1,(t-0.66)*3) }
            case .jet:
                func f(_ x: Float) -> Float { min(max(1.5-abs(4*t-x),0),1) }
                (r,g,b) = (f(3),f(2),f(1))
            }
            pixels[i*4+0] = UInt8(clamping: Int(min(max(r,0),1)*255))
            pixels[i*4+1] = UInt8(clamping: Int(min(max(g,0),1)*255))
            pixels[i*4+2] = UInt8(clamping: Int(min(max(b,0),1)*255))
            pixels[i*4+3] = 255
        }
        let provider = CGDataProvider(data: Data(pixels) as CFData)!
        let cgImage = CGImage(
            width: fitsWidth, height: fitsHeight,
            bitsPerComponent: 8, bitsPerPixel: 32,
            bytesPerRow: fitsWidth * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.last.rawValue),
            provider: provider, decode: nil,
            shouldInterpolate: false, intent: .defaultIntent
        )!
        return UIImage(cgImage: cgImage)
    }

    private func loadFITS(from url: URL) throws {
        let data = try Data(contentsOf: url, options: .mappedIfSafe)
        var offset = 0
        while offset < data.count {
            var header = Data(), str = ""
            repeat {
                let end = min(offset + 2880, data.count)
                header.append(data[offset..<end])
                str = String(data: header, encoding: .ascii) ?? ""
                offset = end
            } while !str.contains("END")
            let cards = str.chunked(into: 80)
            // Build a dictionary of header cards
            var headerDict = [String: String]()
            for card in cards {
                let key = String(card.prefix(8)).trimmingCharacters(in: .whitespaces)
                guard let eqIdx = card.firstIndex(of: "=") else { continue }
                let rawValue = String(card[card.index(after: eqIdx)...])
                    .split(separator: "/")[0]
                    .trimmingCharacters(in: .whitespaces)
                headerDict[key] = rawValue
            }
            // Save header for display
            DispatchQueue.main.async {
                self.headerDict = headerDict
            }
            // Skip non-image HDUs (e.g., binary tables)
            if let xt = headerDict["XTENSION"], !xt.contains("IMAGE") {
                continue
            }
            var w = 0, h = 0, bp = 0
            bscale = 1; bzero = 0
            for card in cards {
                let key = card.prefix(8).trimmingCharacters(in: .whitespaces)
                if key == "END" { break }
                guard let eq = card.firstIndex(of: "=") else { continue }
                let raw = card[card.index(after: eq)...]
                             .split(separator: "/")[0]
                             .trimmingCharacters(in: .whitespaces)
                switch key {
                case "NAXIS1": w = Int(raw) ?? w
                case "NAXIS2": h = Int(raw) ?? h
                case "BITPIX": bp = Int(raw) ?? bp
                case "BSCALE": bscale = Double(raw) ?? bscale
                case "BZERO":  bzero  = Double(raw) ?? bzero
                default: break
                }
            }
            if w <= 0 || h <= 0 || bp == 0 { continue }
            let per = abs(bp) / 8, cnt = w * h * per
            guard offset + cnt <= data.count else {
                throw NSError(domain: "FITS", code: -2,
                              userInfo: [NSLocalizedDescriptionKey: "Image data incomplete"])
            }
            let slice = data[offset..<(offset+cnt)]
            fitsWidth = w; fitsHeight = h
            var floats = [Float](repeating: 0, count: w*h)
            slice.withUnsafeBytes { buf in
                guard let base = buf.baseAddress else { return }
                DispatchQueue.concurrentPerform(iterations: w*h) { i in
                    let ptr = base.advanced(by: i * per)
                    let rv: Float
                    switch bp {
                    case 8: rv = Float(ptr.load(as: UInt8.self))
                    case 16:
                        let v = UInt16(bigEndian: ptr.load(as: UInt16.self))
                        rv = Float(Int16(bitPattern: v))
                    case 32:
                        let v = UInt32(bigEndian: ptr.load(as: UInt32.self))
                        rv = Float(Int32(bitPattern: v))
                    case -32:
                        let v = UInt32(bigEndian: ptr.load(as: UInt32.self))
                        rv = Float(bitPattern: v)
                    default: rv = 0
                    }
                    floats[i] = rv
                }
            }
            fitsFloats = floats
            return
        }
        throw NSError(domain: "FITS", code: -1,
                      userInfo: [NSLocalizedDescriptionKey: "No 2D image HDU found"])
    }
}

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
