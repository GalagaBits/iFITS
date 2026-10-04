//
//  FITSRegion.swift
//  iFITS Start
//
//  Regions (R): shapes, colors and one region in FITS pixel coordinates.
//

import SwiftUI

//
// Regions are stored in FITS pixel coordinates — DS9's "image" coordinates: 1-based, pixel
// centers on whole numbers, y counting up from the bottom row. That keeps them locked to the
// image at every zoom, and they can be written straight into DS9 .reg files.

nonisolated enum RegionShape: String, CaseIterable, Identifiable, Sendable {
    case ellipse, rectangle, line, point

    var id: String { rawValue }

    var title: String {
        switch self {
        case .ellipse: "Ellipse"
        case .rectangle: "Rectangle"
        case .line: "Line"
        case .point: "Point"
        }
    }

    var symbol: String {
        switch self {
        case .ellipse: "oval"
        case .rectangle: "rectangle"
        case .line: "line.diagonal"
        case .point: "smallcircle.filled.circle"
        }
    }

    /// Statistics are measured for ellipses, rectangles and points, not lines.
    var hasStatistics: Bool { self != .line }
}

/// Region colors. The names are DS9's, so .reg files stay readable.
nonisolated struct RegionColor: Identifiable, Sendable {
    let name: String
    let hex: String       // "#RRGGBB"
    var id: String { hex }

    static let palette: [RegionColor] = [
        RegionColor(name: "green", hex: "#00FF00"),
        RegionColor(name: "red", hex: "#FF0000"),
        RegionColor(name: "cyan", hex: "#00FFFF"),
        RegionColor(name: "yellow", hex: "#FFFF00"),
        RegionColor(name: "magenta", hex: "#FF00FF"),
        RegionColor(name: "blue", hex: "#0000FF"),
        RegionColor(name: "white", hex: "#FFFFFF"),
        RegionColor(name: "orange", hex: "#FFA500")
    ]

    /// Color names iFITS understands when reading .reg files.
    private static let named: [String: String] = [
        "green": "#00FF00", "red": "#FF0000", "cyan": "#00FFFF", "yellow": "#FFFF00",
        "magenta": "#FF00FF", "blue": "#0000FF", "white": "#FFFFFF", "black": "#000000",
        "orange": "#FFA500", "pink": "#FFC0CB", "purple": "#A020F0", "gray": "#BEBEBE",
        "grey": "#BEBEBE", "brown": "#A52A2A", "violet": "#EE82EE", "gold": "#FFD700"
    ]

    /// "red", "#f00" or "#ff0000" → "#FF0000".
    static func hex(fromDS9 value: String) -> String? {
        let v = value.trimmingCharacters(in: .whitespaces).lowercased()
        if let hex = named[v] { return hex }
        guard v.hasPrefix("#") else { return nil }
        var digits = String(v.dropFirst())
        if digits.count == 3 { digits = digits.map { "\($0)\($0)" }.joined() }
        guard digits.count == 6, UInt32(digits, radix: 16) != nil else { return nil }
        return "#" + digits.uppercased()
    }

    /// Color for a .reg file: DS9's name when it has one, otherwise "#RRGGBB".
    static func ds9Value(forHex hex: String) -> String {
        palette.first { $0.hex == hex.uppercased() }?.name ?? hex
    }
}

extension Color {
    /// "#RRGGBB" → Color.
    init(regionHex hex: String) {
        let v = UInt32(hex.dropFirst(), radix: 16) ?? 0x00FF00
        self.init(red: Double((v >> 16) & 0xFF) / 255,
                  green: Double((v >> 8) & 0xFF) / 255,
                  blue: Double(v & 0xFF) / 255)
    }
}

/// One region, in FITS pixel coordinates (see the note above).
nonisolated struct FITSRegion: Identifiable, Equatable, Sendable {
    var id = UUID()
    var shape: RegionShape
    var name: String
    var center: CGPoint
    /// Full width × height in pixels. Ellipse: the two diameters. Line: width = length. Point: unused.
    var size: CGSize
    /// Rotation in degrees, counter-clockwise from the +x axis (DS9's angle in image coordinates).
    var angle: Double
    var colorHex: String

    var radians: Double { angle * .pi / 180 }

    /// A line's two ends, in FITS pixels.
    var lineEnds: (start: CGPoint, end: CGPoint) {
        let half = Double(size.width) / 2
        let dx = CGFloat(half * cos(radians)), dy = CGFloat(half * sin(radians))
        return (CGPoint(x: center.x - dx, y: center.y - dy), CGPoint(x: center.x + dx, y: center.y + dy))
    }

    /// Makes this a line from `a` to `b` (FITS pixels).
    mutating func setLine(from a: CGPoint, to b: CGPoint) {
        center = CGPoint(x: (a.x + b.x) / 2, y: (a.y + b.y) / 2)
        size = CGSize(width: hypot(b.x - a.x, b.y - a.y), height: 0)
        angle = FITSRegion.normalized(Double(atan2(b.y - a.y, b.x - a.x)) * 180 / .pi)
    }

    /// Any angle → 0 ..< 360 degrees.
    static func normalized(_ degrees: Double) -> Double {
        guard degrees.isFinite else { return 0 }
        var d = degrees.truncatingRemainder(dividingBy: 360)
        if d < 0 { d += 360 }
        return d >= 360 ? 0 : d
    }
}
