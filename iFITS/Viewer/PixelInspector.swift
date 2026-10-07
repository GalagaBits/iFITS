//
//  PixelInspector.swift
//  iFITS Start
//
//  Pixel inspector: the inspected pixel, its outline and the info panel (with SNR).
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
/// taken from the FITS header (BUNIT, CUNITn, CTYPEn, RADESYS, EQUINOX), plus SNR and noise
/// when an SNR file is open.
struct PixelInfoPanel: View {
    let pixel: InspectedPixel
    let value: Float?
    let wcs: WCS
    let header: [String: String]
    /// Image height in pixels (to convert display rows to FITS y).
    let imageHeight: Int
    /// Channel shown on axis 3 (0-based), for cubes.
    var channel = 0
    /// Signal-to-noise ratio at this pixel, once an SNR file (made in S mode) is open.
    var snr: Float? = nil
    /// Full table, or just "(x, y) pix" and the value (the chevron switches).
    @Binding var expanded: Bool
    /// Shown in a popover (from the one-line Pixel Info of a small window): no glass of its own,
    /// no collapse button.
    var inPopover = false

    @Environment(\.accessibilityReduceMotion) private var reduceMotion

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
                Spacer(minLength: 12)
                if !inPopover {
                    Button {
                        withAnimation(DockAnimation.stage(reduceMotion)) { expanded.toggle() }
                    } label: {
                        Image(systemName: expanded ? "chevron.up" : "chevron.down")
                            .font(.caption.weight(.bold))
                            .foregroundStyle(.secondary)
                            .frame(width: 26, height: 26)
                            .background(.quaternary, in: Circle())
                            .contentShape(Circle())
                    }
                    .buttonStyle(.plain)
                    .hoverEffect(.highlight)
                    .accessibilityLabel(expanded ? "Collapse Pixel Info" : "Expand Pixel Info")
                }
            }

            if expanded || inPopover {
                table
            } else {
                VStack(alignment: .leading, spacing: 0) {
                    compactRow
                    // Keeps the box exactly as wide as when it's expanded.
                    table
                        .hidden()
                        .frame(height: 0)
                        .accessibilityHidden(true)
                }
            }
        }
        .padding(14)
        // Size to the content only, so the box stays compact in the top-right corner.
        .fixedSize()
        .glassEffect(inPopover ? .identity : .regular, in: RoundedRectangle(cornerRadius: 20, style: .continuous))
    }

    /// Collapsed: "(40, 35) pix" and "1555.21 MJy/sr", units smaller and gray.
    private var compactRow: some View {
        let fx = pixel.column + 1, fy = imageHeight - pixel.row
        let unit = text("BUNIT")
        return HStack(alignment: .firstTextBaseline, spacing: 12) {
            Text("(\(fx), \(fy))\(small(" pix"))")
            Spacer(minLength: 12)
            Text("\(valueText)\(small(unit.isEmpty ? "" : " " + unit))")
        }
        .font(.system(.callout, design: .monospaced))
        .lineLimit(1)
    }

    /// A unit in the collapsed row: smaller and gray.
    private func small(_ s: String) -> Text {
        Text(s).font(.system(.caption, design: .monospaced)).foregroundStyle(.secondary)
    }

    /// The full table: pixel, world coordinates, channel, value (and SNR).
    private var table: some View {
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

        // SNR = value / noise, from the SNR extension. The noise follows from the two.
        if let snr {
            rows.append(Row(label: "SNR", value: snr.isNaN ? "NaN" : String(format: "%.4g", Double(snr)), unit: ""))
            if let value, value.isFinite, snr.isFinite, snr != 0 {
                rows.append(Row(label: "Noise", value: String(format: "%.4g", Double(value / snr)), unit: text("BUNIT")))
            }
            if let cut = number("SNRCUT"), snr.isFinite, Double(snr) < cut {
                rows.append(Row(label: "", value: "below SNR " + String(format: "%g", cut), unit: "", secondary: true))
            }
        }
        return rows
    }

    private var valueText: String { Self.valueText(value) }

    /// A pixel value as text: 6 significant digits, "NaN", or "—" (no value).
    static func valueText(_ value: Float?) -> String {
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

/// Pixel Info in a small window: one line, "(x, y) pix" and the value, in a glass pill (like the
/// docks' smallest size). Tap it for the full table.
struct PixelInfoPill: View {
    let pixel: InspectedPixel
    let value: Float?
    /// Image height in pixels (to convert display rows to FITS y).
    let imageHeight: Int
    /// BUNIT.
    let unit: String

    var body: some View {
        // FITS pixel numbers are 1-based, with y counting up from the bottom.
        let fx = pixel.column + 1, fy = imageHeight - pixel.row
        HStack(spacing: 8) {
            Image(systemName: pixel.source.symbol)
                .font(.caption)
                .foregroundStyle(.secondary)
            Text("(\(fx), \(fy))\(small(" pix"))")
                .layoutPriority(1)
            Text("\(PixelInfoPanel.valueText(value))\(small(unit.isEmpty ? "" : " " + unit))")
        }
        .font(.system(.caption, design: .monospaced))
        .lineLimit(1)
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .contentShape(Capsule())
        .glassEffect(.regular.interactive(), in: Capsule())
        .accessibilityElement(children: .combine)
        .accessibilityAddTraits(.isButton)
        .accessibilityHint("Shows the full Pixel Info")
    }

    /// Units: smaller and gray.
    private func small(_ s: String) -> Text {
        Text(s).font(.system(.caption2, design: .monospaced)).foregroundStyle(.secondary)
    }
}
