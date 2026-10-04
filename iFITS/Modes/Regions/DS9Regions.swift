//
//  DS9Regions.swift
//  iFITS Start
//
//  DS9 .reg region files: reading, writing, and the .reg document.
//

import SwiftUI
import UniformTypeIdentifiers

extension WCS {
    /// Size of one pixel on the sky, in arcseconds (celestial WCS).
    var pixelScaleArcsec: Double { sqrt(abs(cd11 * cd22 - cd12 * cd21)) * 3600 }

    /// Direction of celestial north at a pixel, in degrees counter-clockwise from +x
    /// (90 for a north-up image). DS9 measures angles in sky coordinates from there.
    func northAngle(atPixel x: Double, _ y: Double) -> Double {
        guard isCelestial else { return 90 }
        let (ra, dec) = pixelToWorld(x, y)
        let step = max(pixelScaleArcsec / 3600, 1e-7) * 5
        let up = dec + step <= 90
        guard let p = worldToPixel(ra, up ? dec + step : dec - step) else { return 90 }
        var a = atan2(p.1 - y, p.0 - x) * 180 / .pi
        if !up { a += 180 }
        return a
    }
}

/// Reads and writes DS9 region files (https://ds9.si.edu/doc/ref/region.html).
///
/// Written: ellipse, box, line and point, in fk5/icrs (sexagesimal, sizes in ″) when the image
/// has a celestial WCS, otherwise in image pixels. Names go in text={…}, colors in color=.
/// Read: circle (as an ellipse), ellipse, box, line and point, in image, physical, fk5, icrs,
/// j2000 or wcs coordinates. Other shapes and coordinate systems are skipped.
enum DS9Regions {
    static let formatLine = "# Region file format: DS9 version 4.1"
    static let globalLine = "global color=green dashlist=8 3 width=1 font=\"helvetica 10 normal roman\" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1"

    // MARK: Writing

    /// A complete .reg file.
    static func fileText(_ regions: [FITSRegion], wcs: WCS, header: [String: String], fileName: String) -> String {
        var lines = [formatLine, "# Filename: \(fileName)", globalLine]
        if wcs.isCelestial {
            lines.append(skyFrame(header))
            lines += regions.map { skyLine($0, wcs: wcs) }
        } else {
            lines.append("image")
            lines += regions.map { imageLine($0) }
        }
        return lines.joined(separator: "\n") + "\n"
    }

    /// The lines stored in the FITS file (always image pixels, so they never depend on the WCS).
    static func fitsLines(_ regions: [FITSRegion]) -> [String] {
        regions.isEmpty ? [] : [formatLine, globalLine, "image"] + regions.map { imageLine($0) }
    }

    static func imageLine(_ r: FITSRegion) -> String {
        let x = number(r.center.x), y = number(r.center.y)
        let shape: String
        switch r.shape {
        case .ellipse:
            shape = "ellipse(\(x),\(y),\(number(r.size.width / 2)),\(number(r.size.height / 2)),\(number(r.angle)))"
        case .rectangle:
            shape = "box(\(x),\(y),\(number(r.size.width)),\(number(r.size.height)),\(number(r.angle)))"
        case .line:
            let e = r.lineEnds
            shape = "line(\(number(e.start.x)),\(number(e.start.y)),\(number(e.end.x)),\(number(e.end.y)))"
        case .point:
            shape = "point(\(x),\(y))"
        }
        return shape + " # " + properties(r)
    }

    private static func skyLine(_ r: FITSRegion, wcs: WCS) -> String {
        let scale = wcs.pixelScaleArcsec
        func sky(_ p: CGPoint) -> String {
            let w = wcs.pixelToWorld(Double(p.x), Double(p.y))
            return raText(w.0) + "," + decText(w.1)
        }
        func arcsec(_ pixels: CGFloat) -> String { number(Double(pixels) * scale) + "\"" }
        // Sky angles are measured from the sky's x-like axis, 90° from north.
        let north = wcs.northAngle(atPixel: Double(r.center.x), Double(r.center.y))
        let angle = number(FITSRegion.normalized(r.angle - (north - 90)))
        let shape: String
        switch r.shape {
        case .ellipse:
            shape = "ellipse(\(sky(r.center)),\(arcsec(r.size.width / 2)),\(arcsec(r.size.height / 2)),\(angle))"
        case .rectangle:
            shape = "box(\(sky(r.center)),\(arcsec(r.size.width)),\(arcsec(r.size.height)),\(angle))"
        case .line:
            let e = r.lineEnds
            shape = "line(\(sky(e.start)),\(sky(e.end)))"
        case .point:
            shape = "point(\(sky(r.center)))"
        }
        return shape + " # " + properties(r)
    }

    private static func properties(_ r: FITSRegion) -> String {
        var items: [String] = []
        if r.shape == .line { items.append("line=0 0") }
        if r.shape == .point { items.append("point=circle") }
        items.append("color=" + RegionColor.ds9Value(forHex: r.colorHex))
        let name = r.name.replacingOccurrences(of: "{", with: "(").replacingOccurrences(of: "}", with: ")")
        items.append("text={\(name)}")
        return items.joined(separator: " ")
    }

    private static func skyFrame(_ header: [String: String]) -> String {
        let frame = (header["RADESYS"] ?? header["RADECSYS"] ?? "").trimmingCharacters(in: .whitespaces).uppercased()
        return frame == "ICRS" ? "icrs" : "fk5"
    }

    /// Up to 4 decimals, trailing zeros removed.
    static func number(_ v: Double) -> String {
        var s = String(format: "%.4f", v)
        while s.hasSuffix("0") { s.removeLast() }
        if s.hasSuffix(".") { s.removeLast() }
        return s == "-0" ? "0" : s
    }

    /// Right ascension (degrees) as hh:mm:ss.ssss.
    static func raText(_ degrees: Double) -> String {
        var d = degrees.truncatingRemainder(dividingBy: 360)
        if d < 0 { d += 360 }
        let units = Int64((d * 240 * 10_000).rounded())     // 1/10000 s of time
        let h = units / 36_000_000 % 24
        let m = units / 600_000 % 60
        let s = Double(units % 600_000) / 10_000
        return String(format: "%02lld:%02lld:%07.4f", h, m, s)
    }

    /// Declination (degrees) as ±dd:mm:ss.sss.
    static func decText(_ degrees: Double) -> String {
        let units = Int64((abs(degrees) * 3_600_000).rounded())   // milliarcseconds
        let d = units / 3_600_000
        let m = units / 60_000 % 60
        let s = Double(units % 60_000) / 1000
        return (degrees < 0 && units > 0 ? "-" : "+") + String(format: "%02lld:%02lld:%06.3f", d, m, s)
    }

    /// An angle on the sky (arcseconds) in the handiest unit: ″, ′ or °.
    static func angularSize(arcsec: Double) -> String {
        if arcsec < 60 { return String(format: "%.2f″", arcsec) }
        if arcsec < 3600 { return String(format: "%.3f′", arcsec / 60) }
        return String(format: "%.4f°", arcsec / 3600)
    }

    // MARK: Reading

    struct ParseResult {
        var regions: [FITSRegion] = []
        /// Shapes or coordinate systems iFITS can't show.
        var skipped = 0
    }

    private enum System { case image, physical, sky, unsupported }

    /// DS9's "physical" pixels: image = LTM × physical + LTV.
    private struct PhysicalMapping {
        var ltm1 = 1.0, ltm2 = 1.0, ltv1 = 0.0, ltv2 = 0.0

        init(header: [String: String]) {
            func num(_ key: String) -> Double? {
                header[key].flatMap { Double($0.trimmingCharacters(in: .whitespaces)) }
            }
            ltm1 = num("LTM1_1") ?? 1
            ltm2 = num("LTM2_2") ?? 1
            ltv1 = num("LTV1") ?? 0
            ltv2 = num("LTV2") ?? 0
        }

        func toImage(_ x: Double, _ y: Double) -> CGPoint {
            CGPoint(x: ltm1 * x + ltv1, y: ltm2 * y + ltv2)
        }

        var scale: Double { abs(ltm1) }
    }

    static func parse(_ text: String, wcs: WCS, header: [String: String]) -> ParseResult {
        var result = ParseResult()
        var system = System.physical          // DS9's default when a file doesn't say
        var defaultColor = RegionColor.palette[0].hex
        let physical = PhysicalMapping(header: header)

        for line in text.components(separatedBy: .newlines) {
            if line.trimmingCharacters(in: .whitespaces).hasPrefix("#") { continue }   // comment
            for statement in splitStatements(line) {
                var s = statement.trimmingCharacters(in: .whitespaces)
                guard !s.isEmpty, !s.hasPrefix("#") else { continue }

                // "shape(args) # properties"
                var props = ""
                if let hash = s.firstIndex(of: "#") {
                    props = String(s[s.index(after: hash)...])
                    s = String(s[..<hash]).trimmingCharacters(in: .whitespaces)
                }
                let lower = s.lowercased()
                if lower.hasPrefix("global") {
                    if let c = property("color", in: s).flatMap({ RegionColor.hex(fromDS9: $0) }) { defaultColor = c }
                    continue
                }
                if let newSystem = coordinateSystem(lower) {
                    system = newSystem
                    continue
                }
                if s.hasPrefix("+") || s.hasPrefix("-") {      // include / exclude
                    s.removeFirst()
                    s = s.trimmingCharacters(in: .whitespaces)
                }

                let nameEnd = s.firstIndex(where: { $0 == "(" || $0.isWhitespace }) ?? s.endIndex
                var shape = s[..<nameEnd].lowercased()
                var rest = String(s[nameEnd...]).trimmingCharacters(in: .whitespaces)
                // Older files: "circle point 100 100".
                if rest.lowercased().hasPrefix("point"),
                   ["circle", "box", "diamond", "cross", "x", "arrow", "boxcircle"].contains(shape) {
                    shape = "point"
                    rest = String(rest.dropFirst(5))
                }
                if let open = rest.firstIndex(of: "(") {
                    let afterOpen = rest.index(after: open)
                    let close = rest[afterOpen...].lastIndex(of: ")") ?? rest.endIndex
                    rest = String(rest[afterOpen..<close])
                }
                let args = rest.split(whereSeparator: { $0 == "," || $0.isWhitespace }).map(String.init)

                guard system != .unsupported,
                      var region = makeRegion(shape, args, system: system, wcs: wcs, physical: physical),
                      region.center.x.isFinite, region.center.y.isFinite,
                      region.size.width.isFinite, region.size.height.isFinite, region.angle.isFinite else {
                    result.skipped += 1
                    continue
                }
                region.colorHex = property("color", in: props).flatMap { RegionColor.hex(fromDS9: $0) } ?? defaultColor
                region.name = property("text", in: props)?.trimmingCharacters(in: .whitespaces) ?? ""
                result.regions.append(region)
            }
        }
        return result
    }

    /// Splits "fk5;circle(…);box(…)" at semicolons (not inside {…} text).
    private static func splitStatements(_ line: String) -> [String] {
        var parts: [String] = []
        var current = ""
        var depth = 0
        for ch in line {
            if ch == "{" { depth += 1 } else if ch == "}" { depth = max(0, depth - 1) }
            if ch == ";" && depth == 0 {
                parts.append(current)
                current = ""
            } else {
                current.append(ch)
            }
        }
        parts.append(current)
        return parts
    }

    private static func coordinateSystem(_ word: String) -> System? {
        switch word {
        case "image": return .image
        case "physical": return .physical
        case "fk5", "j2000", "icrs", "wcs": return .sky
        case "fk4", "b1950", "galactic", "ecliptic", "linear", "amplifier", "detector": return .unsupported
        default:
            if word.count == 4, word.hasPrefix("wcs") { return .unsupported }     // wcsa … wcsz
            return nil
        }
    }

    private static func makeRegion(_ shape: String, _ args: [String], system: System,
                                   wcs: WCS, physical: PhysicalMapping) -> FITSRegion? {
        func position(_ i: Int) -> CGPoint? {
            guard args.count > i + 1 else { return nil }
            return point(args[i], args[i + 1], system: system, wcs: wcs, physical: physical)
        }
        func length(_ i: Int) -> CGFloat? {
            guard args.count > i else { return nil }
            return pixels(args[i], system: system, wcs: wcs, physical: physical)
        }
        func rotation(_ i: Int, at center: CGPoint) -> Double {
            guard args.count > i, let a = angleValue(args[i]) else { return 0 }
            guard system == .sky else { return FITSRegion.normalized(a) }
            let north = wcs.northAngle(atPixel: Double(center.x), Double(center.y))
            return FITSRegion.normalized(a + north - 90)
        }
        func make(_ shape: RegionShape, _ center: CGPoint, _ size: CGSize, _ angle: Double) -> FITSRegion {
            FITSRegion(shape: shape, name: "", center: center, size: size, angle: angle, colorHex: "")
        }

        switch shape {
        case "circle":
            guard let c = position(0), let r = length(2), r > 0 else { return nil }
            return make(.ellipse, c, CGSize(width: 2 * r, height: 2 * r), 0)
        case "ellipse":
            // ellipse x y r1 r2 [angle]   (an elliptical annulus keeps only its first radii)
            guard let c = position(0), let r1 = length(2), let r2 = length(3), r1 > 0, r2 > 0 else { return nil }
            return make(.ellipse, c, CGSize(width: 2 * r1, height: 2 * r2),
                        args.count >= 5 ? rotation(args.count - 1, at: c) : 0)
        case "box":
            guard let c = position(0), let w = length(2), let h = length(3), w > 0, h > 0 else { return nil }
            return make(.rectangle, c, CGSize(width: w, height: h),
                        args.count >= 5 ? rotation(args.count - 1, at: c) : 0)
        case "line":
            guard let a = position(0), let b = position(2) else { return nil }
            var r = make(.line, a, .zero, 0)
            r.setLine(from: a, to: b)
            return r
        case "point":
            guard let c = position(0) else { return nil }
            return make(.point, c, .zero, 0)
        default:
            return nil
        }
    }

    /// A position → FITS image pixel.
    private static func point(_ xs: String, _ ys: String, system: System,
                              wcs: WCS, physical: PhysicalMapping) -> CGPoint? {
        let xl = xs.lowercased(), yl = ys.lowercased()
        // Explicit pixel units work in any coordinate system.
        if xl.hasSuffix("i"), yl.hasSuffix("i"), let x = Double(xl.dropLast()), let y = Double(yl.dropLast()) {
            return CGPoint(x: x, y: y)
        }
        if xl.hasSuffix("p"), yl.hasSuffix("p"), let x = Double(xl.dropLast()), let y = Double(yl.dropLast()) {
            return physical.toImage(x, y)
        }
        switch system {
        case .image:
            guard let x = Double(xs), let y = Double(ys) else { return nil }
            return CGPoint(x: x, y: y)
        case .physical:
            guard let x = Double(xs), let y = Double(ys) else { return nil }
            return physical.toImage(x, y)
        case .sky:
            guard wcs.isCelestial,
                  let ra = parseSky(xs, isRA: true), let dec = parseSky(ys, isRA: false),
                  let p = wcs.worldToPixel(ra, dec) else { return nil }
            return CGPoint(x: p.0, y: p.1)
        case .unsupported:
            return nil
        }
    }

    /// A size → image pixels. Units: ″ (arcsec), ′ (arcmin), d (degrees), r (radians),
    /// i (image pixels), p (physical pixels). No unit: degrees in sky systems, pixels otherwise.
    private static func pixels(_ token: String, system: System, wcs: WCS, physical: PhysicalMapping) -> CGFloat? {
        let t = token.lowercased()
        let scale = wcs.pixelScaleArcsec
        func sky(_ arcsec: Double?) -> CGFloat? {
            guard let arcsec, wcs.isCelestial, scale > 0 else { return nil }
            return CGFloat(arcsec / scale)
        }
        if t.hasSuffix("\"") { return sky(Double(t.dropLast())) }
        if t.hasSuffix("'") { return sky(Double(t.dropLast()).map { $0 * 60 }) }
        if t.hasSuffix("d") { return sky(Double(t.dropLast()).map { $0 * 3600 }) }
        if t.hasSuffix("r") { return sky(Double(t.dropLast()).map { $0 * 180 / .pi * 3600 }) }
        if t.hasSuffix("i") { return Double(t.dropLast()).map { CGFloat($0) } }
        if t.hasSuffix("p") { return Double(t.dropLast()).map { CGFloat($0 * physical.scale) } }
        guard let v = Double(t) else { return nil }
        switch system {
        case .image: return CGFloat(v)
        case .physical: return CGFloat(v * physical.scale)
        case .sky: return sky(v * 3600)
        case .unsupported: return nil
        }
    }

    private static func angleValue(_ token: String) -> Double? {
        let t = token.lowercased()
        if t.hasSuffix("d") { return Double(t.dropLast()) }
        if t.hasSuffix("r") { return Double(t.dropLast()).map { $0 * 180 / .pi } }
        return Double(t)
    }

    /// Right ascension or declination in degrees, from "05:42:40.26" / "5h42m40.26s" / "85.6677"
    /// (RA) or "+49:52:07.2" / "+49d52m07.2s" / "49.8687" (Dec).
    static func parseSky(_ token: String, isRA: Bool) -> Double? {
        var t = token.trimmingCharacters(in: .whitespaces).lowercased()
            .replacingOccurrences(of: "−", with: "-")
        let sexagesimal = t.contains(":") || (isRA ? t.contains("h") : (t.contains("d") && t.contains("m")))
        guard sexagesimal else {
            if t.hasSuffix("d") || t.hasSuffix("°") { t.removeLast() }
            return Double(t)
        }
        var sign = 1.0
        if t.hasPrefix("-") {
            sign = -1
            t.removeFirst()
        } else if t.hasPrefix("+") {
            t.removeFirst()
        }
        let parts = t.split(whereSeparator: { ":hdms°'\"″′ ".contains($0) }).compactMap { Double($0) }
        guard !parts.isEmpty else { return nil }
        var value = parts[0]
        if parts.count > 1 { value += parts[1] / 60 }
        if parts.count > 2 { value += parts[2] / 3600 }
        return sign * value * (isRA ? 15 : 1)
    }

    /// The value of `key=` in a DS9 property list ({…}, "…" and '…' values included).
    static func property(_ key: String, in text: String) -> String? {
        var searchStart = text.startIndex
        while let r = text.range(of: key + "=", options: .caseInsensitive, range: searchStart..<text.endIndex) {
            // Must be a whole key: at the start, or after a space.
            if r.lowerBound != text.startIndex, !text[text.index(before: r.lowerBound)].isWhitespace {
                searchStart = r.upperBound
                continue
            }
            let rest = text[r.upperBound...]
            guard let first = rest.first else { return nil }
            let closer: Character? = first == "{" ? "}" : (first == "\"" || first == "'") ? first : nil
            if let closer {
                let body = rest.dropFirst()
                let end = body.firstIndex(of: closer) ?? body.endIndex
                return String(body[..<end])
            }
            let end = rest.firstIndex(where: { $0.isWhitespace }) ?? rest.endIndex
            return String(rest[..<end])
        }
        return nil
    }
}

/// A .reg file's text, for "Export Regions".
nonisolated struct RegionFileDocument: FileDocument {
    static let regionType = UTType(filenameExtension: "reg", conformingTo: .plainText) ?? .plainText
    static var readableContentTypes: [UTType] { [regionType, .plainText] }

    var text: String

    init(text: String) {
        self.text = text
    }

    init(configuration: ReadConfiguration) throws {
        text = String(decoding: configuration.file.regularFileContents ?? Data(), as: UTF8.self)
    }

    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        FileWrapper(regularFileWithContents: Data(text.utf8))
    }
}
