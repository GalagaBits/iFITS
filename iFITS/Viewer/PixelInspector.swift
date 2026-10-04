//
//  PixelInspector.swift
//  iFITS Start
//
//  Pixel inspector: the inspected pixel, its outline and the info panel.
//

import SwiftUI

enum InspectSource: Equatable {
    case pointer, pencil, touch

    var symbol: String {
        switch self {
        case .pointer: "cursorarrow"
        case .pencil: "applepencil"
        case .touch: "hand.tap"
        }
    }
}

/// 0-based column / row in the loaded image (row 0 = first row in the file).
struct InspectedPixel: Equatable {
    let column: Int
    let row: Int
    let source: InspectSource
}

/// Outline around the inspected pixel, drawn once the pixel is big enough to see.
struct InspectedPixelOutline: View {
    let rect: CGRect

    var body: some View {
        ZStack {
            if rect.width >= 4 {
                Rectangle()
                    .stroke(Color.black.opacity(0.7), lineWidth: 3)
                    .overlay(Rectangle().stroke(Color.white, lineWidth: 1.5))
                    .frame(width: rect.width, height: rect.height)
                    .position(x: rect.midX, y: rect.midY)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .allowsHitTesting(false)
    }
}

/// Liquid Glass box with the pixel's coordinates, world coordinates and value, with units
/// taken from the FITS header (BUNIT, CUNITn, CTYPEn, RADESYS, EQUINOX).
struct PixelInfoPanel: View {
    let pixel: InspectedPixel
    let value: Float?
    let wcs: WCS
    let header: [String: String]
    /// Image height in pixels (to convert display rows to FITS y).
    let imageHeight: Int
    /// Channel shown on axis 3 (0-based), for cubes.
    var channel = 0

    private struct Row {
        let label: String
        let value: String
        let unit: String
        var secondary = false
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 6) {
                Image(systemName: pixel.source.symbol)
                    .foregroundStyle(.secondary)
                Text("Pixel Info")
                    .font(.subheadline.weight(.semibold))
            }

            Grid(alignment: .leading, horizontalSpacing: 10, verticalSpacing: 3) {
                ForEach(Array(rows.enumerated()), id: \.offset) { _, row in
                    GridRow {
                        Text(row.label)
                            .foregroundStyle(.secondary)
                        Text(row.value)
                            .foregroundStyle(row.secondary ? .secondary : .primary)
                            .gridColumnAlignment(.trailing)
                        Text(row.unit)
                            .foregroundStyle(.secondary)
                    }
                    .font(.system(row.secondary ? .caption : .callout, design: .monospaced))
                }
            }
        }
        .padding(14)
        // Size to the content only, so the box stays compact in the top-right corner.
        .fixedSize()
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 20, style: .continuous))
        .allowsHitTesting(false)
    }

    // MARK: Content

    private var rows: [Row] {
        var rows: [Row] = []
        // FITS pixel numbers are 1-based.
        // Display row 0 is the top; the image is flipped, so FITS y counts up from the bottom.
        let fx = pixel.column + 1, fy = imageHeight - pixel.row
        rows.append(Row(label: "Pixel", value: "\(fx), \(fy)", unit: "px"))

        if header["CTYPE1"] != nil || header["CTYPE2"] != nil {
            let (w1, w2) = wcs.pixelToWorld(Double(fx), Double(fy))
            if wcs.isCelestial {
                rows.append(Row(label: "RA", value: Self.hms(w1), unit: "h:m:s"))
                rows.append(Row(label: "", value: String(format: "%.6f", w1), unit: "deg", secondary: true))
                rows.append(Row(label: "Dec", value: Self.dms(w2), unit: "d:m:s"))
                rows.append(Row(label: "", value: String(format: "%+.6f", w2), unit: "deg", secondary: true))
                if let frame = frameName {
                    rows.append(Row(label: "Frame", value: frame, unit: ""))
                }
            } else {
                rows.append(Row(label: axisLabel(1), value: String(format: "%.6g", w1), unit: text("CUNIT1")))
                rows.append(Row(label: axisLabel(2), value: String(format: "%.6g", w2), unit: text("CUNIT2")))
            }
        }

        if let spectral = spectralRow { rows.append(spectral) }

        rows.append(Row(label: "Value", value: valueText, unit: text("BUNIT")))
        return rows
    }

    private var valueText: String {
        guard let value else { return "—" }
        if value.isNaN { return "NaN" }
        return String(format: "%.6g", Double(value))
    }

    private func text(_ key: String) -> String {
        header[key]?.trimmingCharacters(in: .whitespaces) ?? ""
    }

    private func number(_ key: String) -> Double? {
        Double(text(key).replacingOccurrences(of: "D", with: "E"))
    }

    private func axisLabel(_ n: Int) -> String {
        let ctype = text("CTYPE\(n)")
        return ctype.isEmpty ? "Axis \(n)" : ctype
    }

    /// Celestial frame from RADESYS (or the older RADECSYS) and EQUINOX.
    private var frameName: String? {
        var frame = text("RADESYS")
        if frame.isEmpty { frame = text("RADECSYS") }
        let equinox = number("EQUINOX")
        if frame.isEmpty, equinox != nil { frame = "FK5" }
        guard !frame.isEmpty else { return nil }
        if let equinox, frame.hasPrefix("FK") {
            frame += String(format: " %@%g", frame == "FK4" ? "B" : "J", equinox)
        }
        return frame
    }

    /// Third axis of a cube (e.g. wavelength) for the plane being shown.
    private var spectralRow: Row? {
        guard (Int(text("NAXIS")) ?? 0) >= 3 else { return nil }
        let ctype = text("CTYPE3").uppercased()
        guard !ctype.isEmpty else { return nil }
        let crval = number("CRVAL3") ?? 0
        let crpix = number("CRPIX3") ?? 1
        let step = number("CD3_3") ?? ((number("CDELT3") ?? 1) * (number("PC3_3") ?? 1))
        let world = crval + (Double(channel + 1) - crpix) * step

        let label: String
        switch String(ctype.prefix(4)) {
        case "WAVE": label = "Wavelength"
        case "AWAV": label = "Air wavelength"
        case "FREQ": label = "Frequency"
        case "VRAD", "VOPT", "VELO": label = "Velocity"
        case "ZOPT": label = "Redshift"
        case "ENER": label = "Energy"
        default: label = ctype.trimmingCharacters(in: CharacterSet(charactersIn: "-"))
        }
        return Row(label: label, value: String(format: "%.6g", world), unit: text("CUNIT3"))
    }

    // MARK: Formatting

    /// Right ascension (degrees) as hh:mm:ss.sss.
    static func hms(_ degrees: Double) -> String {
        var d = degrees.truncatingRemainder(dividingBy: 360)
        if d < 0 { d += 360 }
        let ms = Int64((d * 240 * 1000).rounded())        // milliseconds of time
        let h = (ms / 3_600_000) % 24
        let m = (ms / 60_000) % 60
        let s = Double(ms % 60_000) / 1000
        return String(format: "%02ld:%02ld:%06.3f", Int(h), Int(m), s)
    }

    /// Declination (degrees) as ±dd:mm:ss.ss.
    static func dms(_ degrees: Double) -> String {
        let cs = Int64((abs(degrees) * 3600 * 100).rounded())   // centi-arcseconds
        let d = cs / 360_000
        let m = (cs / 6_000) % 60
        let s = Double(cs % 6_000) / 100
        let sign = (degrees < 0 && cs > 0) ? "−" : "+"
        return sign + String(format: "%02ld:%02ld:%05.2f", Int(d), Int(m), s)
    }
}
