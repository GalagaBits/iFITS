//
//  ARAxisTicks.swift
//  iFITS Start
//
//  Tick marks, labels and grid lines for the 3-D box: RA and Dec from the WCS, and the depth
//  axis (wavelength, frequency, velocity, …) in proper units.
//

import Foundation
import simd

/// One tick. `position` is in box coordinates: 0…1 on each axis (x = NAXIS1, y = NAXIS2,
/// z = depth axis), with (0, 0, 0) at pixel (0.5, 0.5) of the first channel.
struct ARTick {
    let position: SIMD3<Float>
    let text: String
}

struct ARAxisInfo {
    let title: String
    let ticks: [ARTick]
    /// Direction (box coordinates) labels and tick marks stick out of the box.
    let outward: SIMD3<Float>
    let titlePosition: SIMD3<Float>
}

/// Ticks for the three labelled edges, plus grid lines on the three faces that meet at (0, 0, 0).
struct ARAxisSet {
    let x: ARAxisInfo
    let y: ARAxisInfo
    let z: ARAxisInfo
    /// Polylines in box coordinates.
    let gridLines: [[SIMD3<Float>]]
}

enum ARAxisTicks {
    /// - extentX, extentY: image pixels the box covers; extentZ: channels along the depth axis.
    static func make(wcs: WCS, header: [String: String], extentX: Int, extentY: Int, extentZ: Int,
                     depthAxis: CubeAxis) -> ARAxisSet {
        // Labelled edges: x along the bottom front, y up the left front, depth along the bottom right.
        let xOut = simd_normalize(SIMD3<Float>(0, -1, 1))
        let yOut = simd_normalize(SIMD3<Float>(-1, 0, 1))
        let zOut = simd_normalize(SIMD3<Float>(1, -1, 0))

        var grid: [[SIMD3<Float>]] = []
        let xAxis: ARAxisInfo
        let yAxis: ARAxisInfo

        let celestial = wcs.isCelestial && header["CTYPE1"] != nil && header["CTYPE2"] != nil
        if celestial {
            let sky = SkyEdges(wcs: wcs, extentX: extentX, extentY: extentY)
            xAxis = ARAxisInfo(title: sky.xKind.title, ticks: sky.xTicks.map {
                ARTick(position: SIMD3<Float>(Float($0.u), 0, 1), text: $0.text)
            }, outward: xOut, titlePosition: SIMD3<Float>(0.5, 0, 1))
            yAxis = ARAxisInfo(title: sky.yKind.title, ticks: sky.yTicks.map {
                ARTick(position: SIMD3<Float>(0, Float($0.u), 1), text: $0.text)
            }, outward: yOut, titlePosition: SIMD3<Float>(0, 0.5, 1))
            grid += sky.backFaceCurves
            for t in sky.xTicks { grid.append([SIMD3(Float(t.u), 0, 0), SIMD3(Float(t.u), 0, 1)]) }
            for t in sky.yTicks { grid.append([SIMD3(0, Float(t.u), 0), SIMD3(0, Float(t.u), 1)]) }
        } else {
            let xt = pixelTicks(extent: extentX), yt = pixelTicks(extent: extentY)
            xAxis = ARAxisInfo(title: "x (pixel)", ticks: xt.map { ARTick(position: SIMD3(Float($0.u), 0, 1), text: $0.text) },
                               outward: xOut, titlePosition: SIMD3<Float>(0.5, 0, 1))
            yAxis = ARAxisInfo(title: "y (pixel)", ticks: yt.map { ARTick(position: SIMD3(0, Float($0.u), 1), text: $0.text) },
                               outward: yOut, titlePosition: SIMD3<Float>(0, 0.5, 1))
            for t in xt {
                grid.append([SIMD3(Float(t.u), 0, 0), SIMD3(Float(t.u), 0, 1)])
                grid.append([SIMD3(Float(t.u), 0, 0), SIMD3(Float(t.u), 1, 0)])
            }
            for t in yt {
                grid.append([SIMD3(0, Float(t.u), 0), SIMD3(0, Float(t.u), 1)])
                grid.append([SIMD3(0, Float(t.u), 0), SIMD3(1, Float(t.u), 0)])
            }
        }

        let depth = depthTicks(axis: depthAxis, extent: extentZ)
        let zAxis = ARAxisInfo(title: depth.title, ticks: depth.ticks.map {
            ARTick(position: SIMD3<Float>(1, 0, Float($0.u)), text: $0.text)
        }, outward: zOut, titlePosition: SIMD3<Float>(1, 0, 0.5))
        for t in depth.ticks {
            let w = Float(t.u)
            grid.append([SIMD3(0, 0, w), SIMD3(1, 0, w)])
            grid.append([SIMD3(0, 0, w), SIMD3(0, 1, w)])
        }
        return ARAxisSet(x: xAxis, y: yAxis, z: zAxis, gridLines: grid)
    }

    // MARK: Pixel axes (no celestial WCS)

    private static func pixelTicks(extent: Int) -> [(u: Double, text: String)] {
        let (values, step) = NiceTicks.values(lo: 1, hi: Double(extent), target: 4)
        return values.map { (u: ($0 - 0.5) / Double(extent), text: NiceTicks.label($0, step: step)) }
    }

    // MARK: Depth axis

    /// Ticks for the depth axis in a readable unit: GHz / MHz, km/s, µm / nm, or the header's unit.
    static func depthTicks(axis: CubeAxis, extent: Int) -> (title: String, ticks: [(u: Double, text: String)]) {
        let n = max(1, extent)
        if axis.kind == .stokes {
            let names: [Int: String] = [1: "I", 2: "Q", 3: "U", 4: "V", -1: "RR", -2: "LL", -3: "RL", -4: "LR",
                                        -5: "XX", -6: "YY", -7: "XY", -8: "YX"]
            let ticks = (0..<min(axis.length, n)).map { i -> (u: Double, text: String) in
                let code = Int(axis.world(at: i).rounded())
                return (u: (Double(i) + 0.5) / Double(n), text: names[code] ?? "\(code)")
            }
            return ("Stokes", ticks)
        }

        let (factor, unit, name) = displayUnit(axis)
        // World value at the two outer channel edges, in the display unit.
        let a = axis.world(at: 0) * factor - 0.5 * axis.cdelt * factor
        let b = axis.world(at: n - 1) * factor + 0.5 * axis.cdelt * factor
        let (values, step) = NiceTicks.values(lo: min(a, b), hi: max(a, b), target: 4)
        guard axis.cdelt != 0, factor != 0 else {
            return ("Channel", channelTicks(n))
        }
        let ticks = values.compactMap { v -> (u: Double, text: String)? in
            // Channel (0-based) where the world value is v.
            let index = (v / factor - axis.crval) / axis.cdelt + axis.crpix - 1
            let u = (index + 0.5) / Double(n)
            guard u >= -1e-6, u <= 1 + 1e-6 else { return nil }
            return (u: u, text: NiceTicks.label(v, step: step))
        }
        let title = unit.isEmpty ? name : "\(name) (\(unit))"
        return (title, ticks)
    }

    private static func channelTicks(_ n: Int) -> [(u: Double, text: String)] {
        let (values, step) = NiceTicks.values(lo: 0, hi: Double(n - 1), target: 4)
        return values.map { (u: ($0 + 0.5) / Double(n), text: NiceTicks.label($0, step: step)) }
    }

    /// (multiply header values by, unit to show, axis name).
    private static func displayUnit(_ axis: CubeAxis) -> (Double, String, String) {
        let unit = axis.cunit.lowercased().replacingOccurrences(of: " ", with: "")
        let lo = axis.world(at: 0), hi = axis.world(at: max(0, axis.length - 1))
        let biggest = max(abs(lo), abs(hi))
        switch String(axis.ctype.prefix(4)) {
        case "FREQ":
            let hz: Double = ["khz": 1e3, "mhz": 1e6, "ghz": 1e9][unit] ?? 1
            let f = biggest * hz
            if f >= 1e9 { return (hz / 1e9, "GHz", "Frequency") }
            if f >= 1e6 { return (hz / 1e6, "MHz", "Frequency") }
            if f >= 1e3 { return (hz / 1e3, "kHz", "Frequency") }
            return (hz, "Hz", "Frequency")
        case "VRAD", "VOPT", "VELO", "FELO":
            let ms: Double = ["km/s": 1000, "kms-1": 1000, "km.s-1": 1000, "km/sec": 1000][unit] ?? 1
            return (ms / 1000, "km/s", "Velocity")
        case "WAVE", "AWAV":
            let m: Double = ["cm": 1e-2, "mm": 1e-3, "um": 1e-6, "micron": 1e-6, "microns": 1e-6,
                             "µm": 1e-6, "μm": 1e-6, "nm": 1e-9, "angstrom": 1e-10, "angstroms": 1e-10,
                             "a": 1e-10, "å": 1e-10][unit] ?? 1
            let l = biggest * m
            let name = axis.ctype.hasPrefix("AWAV") ? "Air wavelength" : "Wavelength"
            if l < 1e-6 { return (m / 1e-9, "nm", name) }
            if l < 1e-3 { return (m / 1e-6, "µm", name) }
            if l < 1 { return (m / 1e-3, "mm", name) }
            return (m, "m", name)
        default:
            let name = axis.ctype.trimmingCharacters(in: CharacterSet(charactersIn: "- "))
            return (1, axis.cunit, name.isEmpty ? "Axis \(axis.number)" : name)
        }
    }
}

// MARK: - RA / Dec ticks along the box edges

/// Ticks where RA and Dec cross round values along the bottom and left edges of the image, and
/// their grid curves on the back face.
private struct SkyEdges {
    enum Kind {
        case ra, dec
        var title: String { self == .ra ? "RA (h:m:s)" : "Dec (d:m:s)" }
    }

    let xKind: Kind
    let yKind: Kind
    var xTicks: [(u: Double, text: String)] = []
    var yTicks: [(u: Double, text: String)] = []
    var backFaceCurves: [[SIMD3<Float>]] = []

    private static let samples = 160
    private static let timeSteps: [Double] = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900,
                                              1200, 1800, 3600, 7200, 10800, 21600]   // seconds of time
    private static let arcSteps: [Double] = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900,
                                             1200, 1800, 3600, 7200, 18000, 36000]    // arcseconds

    @MainActor
    init(wcs: WCS, extentX: Int, extentY: Int) {
        let n = Self.samples
        let w = Double(extentX), h = Double(extentY)
        // Pixel coordinates (1-based) for box coordinates u, v (0…1): edges at 0.5 and N + 0.5.
        func world(_ u: Double, _ v: Double) -> (Double, Double) {
            wcs.pixelToWorld(0.5 + u * w, 0.5 + v * h)
        }
        let center = world(0.5, 0.5)
        // RA without the jump at 0°/360°, relative to the center.
        func ra(_ value: Double) -> Double {
            var d = (value - center.0).truncatingRemainder(dividingBy: 360)
            if d > 180 { d -= 360 }
            if d < -180 { d += 360 }
            return center.0 + d
        }
        func coordinate(_ kind: Kind, _ u: Double, _ v: Double) -> Double {
            let p = world(u, v)
            return kind == .ra ? ra(p.0) : p.1
        }

        // Which coordinate changes most along the bottom edge goes on x (usually RA).
        let a = world(0, 0), b = world(1, 0)
        let raChange = abs(ra(b.0) - ra(a.0)) * cos(center.1 * .pi / 180)
        let decChange = abs(b.1 - a.1)
        let xk: Kind = raChange >= decChange ? .ra : .dec
        let yk: Kind = xk == .ra ? .dec : .ra
        xKind = xk
        yKind = yk

        let xEdge = (0...n).map { coordinate(xk, Double($0) / Double(n), 0) }
        let yEdge = (0...n).map { coordinate(yk, 0, Double($0) / Double(n)) }
        let (xValues, xStep) = Self.roundValues(xEdge, kind: xk)
        let (yValues, yStep) = Self.roundValues(yEdge, kind: yk)

        var xt: [(u: Double, text: String)] = []
        for v in xValues {
            if let u = Self.crossing(xEdge, v) { xt.append((u: u, text: Self.format(v, kind: xk, step: xStep))) }
        }
        var yt: [(u: Double, text: String)] = []
        for v in yValues {
            if let u = Self.crossing(yEdge, v) { yt.append((u: u, text: Self.format(v, kind: yk, step: yStep))) }
        }
        xTicks = xt
        yTicks = yt

        // Back face (depth 0): follow each round value across the image. The x coordinate is
        // found along each row, the y coordinate along each column.
        let m = 48
        var curves: [[SIMD3<Float>]] = []
        for v in xValues {
            var line: [SIMD3<Float>] = []
            for r in 0...m {
                let rv = Double(r) / Double(m)
                let row = (0...m).map { coordinate(xk, Double($0) / Double(m), rv) }
                if let u = Self.crossing(row, v) {
                    line.append(SIMD3(Float(u), Float(rv), 0))
                } else {
                    if line.count > 1 { curves.append(line) }
                    line = []
                }
            }
            if line.count > 1 { curves.append(line) }
        }
        for v in yValues {
            var line: [SIMD3<Float>] = []
            for c in 0...m {
                let cu = Double(c) / Double(m)
                let column = (0...m).map { coordinate(yk, cu, Double($0) / Double(m)) }
                if let u = Self.crossing(column, v) {
                    line.append(SIMD3(Float(cu), Float(u), 0))
                } else {
                    if line.count > 1 { curves.append(line) }
                    line = []
                }
            }
            if line.count > 1 { curves.append(line) }
        }
        backFaceCurves = curves
    }

    /// Where (0…1 along the samples) the samples first pass through `value`.
    private static func crossing(_ s: [Double], _ value: Double) -> Double? {
        guard s.count > 1 else { return nil }
        for i in 0..<(s.count - 1) {
            let a = s[i], b = s[i + 1]
            guard a != b, (a - value) * (b - value) <= 0 else { continue }
            let f = (value - a) / (b - a)
            return (Double(i) + f) / Double(s.count - 1)
        }
        return nil
    }

    /// Round values (degrees) inside the range of the samples, and their step.
    private static func roundValues(_ s: [Double], kind: Kind) -> ([Double], Double) {
        guard let lo = s.min(), let hi = s.max(), hi > lo else { return ([], 1) }
        let unit = kind == .ra ? 240.0 : 3600.0        // seconds of time / arcseconds per degree
        let target = (hi - lo) / 4 * unit
        let table = kind == .ra ? timeSteps : arcSteps
        let stepUnits: Double
        if let first = table.first, let last = table.last, target >= first, target <= last,
           let c = table.first(where: { $0 >= target }) {
            stepUnits = c
        } else {
            stepUnits = NiceTicks.roundStep(target)
        }
        let step = stepUnits / unit
        var out: [Double] = []
        var v = (lo / step).rounded(.up) * step
        while v <= hi, out.count < 50 {
            out.append(v)
            v += step
        }
        return (out, step)
    }

    private static func format(_ value: Double, kind: Kind, step: Double) -> String {
        let unit = kind == .ra ? 240.0 : 3600.0
        let stepUnits = step * unit
        let d = stepUnits > 0 ? min(6, max(0, Int(ceil(-log10(stepUnits) - 1e-9)))) : 0
        switch kind {
        case .ra:
            var r = value.truncatingRemainder(dividingBy: 360)
            if r < 0 { r += 360 }
            let (a, b, s) = parts(r * 240, decimals: d)
            return String(format: "%02ld:%02ld:", a % 24, b) + s
        case .dec:
            let (a, b, s) = parts(abs(value) * 3600, decimals: d)
            let isZero = a == 0 && b == 0 && (Double(s) ?? 0) == 0
            return ((value < 0 && !isZero) ? "−" : "+") + String(format: "%02ld:%02ld:", a, b) + s
        }
    }

    /// Whole hours / degrees, minutes and a seconds string, rounded once so "60" never shows.
    private static func parts(_ totalSeconds: Double, decimals d: Int) -> (Int, Int, String) {
        let p = pow(10.0, Double(d))
        let q = Int64((totalSeconds * p).rounded())
        let perMin = 60 * Int64(p), perHour = 3600 * Int64(p)
        let a = Int(q / perHour)
        let b = Int((q % perHour) / perMin)
        let sScaled = q % perMin
        let s = d > 0
            ? String(format: "%0\(d + 3).\(d)f", Double(sScaled) / p)
            : String(format: "%02ld", Int(sScaled))
        return (a, b, s)
    }
}
