//
//  CubeAxis.swift
//  iFITS Start
//
//  Cubes (C): spectral / extra axes from the header (CTYPE, CRVAL, CDELT, ...).
//

import SwiftUI

//
// A FITS image with NAXIS ≥ 3 is a cube: NAXIS1 × NAXIS2 planes stacked along NAXIS3 (and NAXIS4,
// …). Only the plane on screen is decoded; the file is memory-mapped, so even very large cubes
// open quickly. Channels are numbered from 0, like CARTA.

/// One axis beyond x and y (NAXIS3, NAXIS4, …), with what's needed to describe each channel.
nonisolated struct CubeAxis: Identifiable, Equatable, Sendable {
    enum Kind: Equatable, Sendable { case spectral, stokes, other }

    let number: Int           // FITS axis number (3, 4, …)
    let length: Int
    let ctype: String
    let cunit: String
    let crval: Double
    let crpix: Double
    let cdelt: Double
    let kind: Kind
    /// Spectral reference frame (SPECSYS, e.g. LSRK), if known.
    let frame: String
    let restFrequency: Double?     // Hz (RESTFRQ / RESTFREQ)
    let restWavelength: Double?    // m (RESTWAV)
    let velref: Int

    var id: Int { number }

    private static let speedOfLight = 299_792_458.0      // m/s

    private static let stokesNames: [Int: String] = [
        1: "I", 2: "Q", 3: "U", 4: "V",
        -1: "RR", -2: "LL", -3: "RL", -4: "LR", -5: "XX", -6: "YY", -7: "XY", -8: "YX"
    ]

    init(number n: Int, length: Int, header h: [String: String]) {
        func text(_ key: String) -> String { (h[key] ?? "").trimmingCharacters(in: .whitespaces) }
        func num(_ key: String) -> Double? { Double(text(key).replacingOccurrences(of: "D", with: "E")) }

        let type = text("CTYPE\(n)").uppercased()
        let ref = Int(text("VELREF")) ?? 0
        let prefix = String(type.prefix(4))
        let axisKind: Kind
        if ["FREQ", "VRAD", "VOPT", "VELO", "FELO", "WAVE", "AWAV", "ZOPT", "ENER", "WAVN"].contains(prefix) {
            axisKind = .spectral
        } else if type.hasPrefix("STOKES") {
            axisKind = .stokes
        } else {
            axisKind = .other
        }

        var specFrame = text("SPECSYS").uppercased()
        if specFrame.isEmpty, axisKind == .spectral {
            // Older files put the frame in CTYPE (VELO-LSR, FREQ-HEL, …) or VELREF.
            if type.contains("LSR") {
                specFrame = "LSRK"
            } else if type.contains("HEL") || type.contains("BAR") {
                specFrame = "BARYCENT"
            } else if type.contains("OBS") || type.contains("TOP") {
                specFrame = "TOPOCENT"
            } else {
                switch ref % 256 {
                case 1: specFrame = "LSRK"
                case 2: specFrame = "BARYCENT"
                case 3: specFrame = "TOPOCENT"
                default: break
                }
            }
        }

        let rf = num("RESTFRQ") ?? num("RESTFREQ")
        let rw = num("RESTWAV") ?? num("RESTWAVE")

        number = n
        self.length = max(1, length)
        ctype = type
        cunit = text("CUNIT\(n)")
        crval = num("CRVAL\(n)") ?? 0
        crpix = num("CRPIX\(n)") ?? 1
        cdelt = num("CD\(n)_\(n)") ?? ((num("CDELT\(n)") ?? 1) * (num("PC\(n)_\(n)") ?? 1))
        velref = ref
        kind = axisKind
        frame = specFrame
        restFrequency = (rf ?? 0) > 0 ? rf : nil
        restWavelength = (rw ?? 0) > 0 ? rw : nil
    }

    /// "Channel" for spectral axes, "Stokes", or the axis type from the header.
    var name: String {
        switch kind {
        case .spectral: "Channel"
        case .stokes: "Stokes"
        case .other: ctype.isEmpty ? "Axis \(number)" : ctype
        }
    }

    /// World coordinate of a (0-based) channel.
    func world(at index: Int) -> Double {
        crval + (Double(index + 1) - crpix) * cdelt
    }

    /// Lines describing a channel, e.g. ["LSRK", "25.6409 GHz", "530.0000 km/s"].
    func info(at index: Int) -> [String] {
        let w = world(at: index)
        switch kind {
        case .stokes:
            let code = Int(w.rounded())
            return ["Stokes " + (Self.stokesNames[code] ?? "\(code)")]
        case .other:
            return [String(format: "%.6g", w) + (cunit.isEmpty ? "" : " " + cunit)]
        case .spectral:
            var lines: [String] = []
            if !frame.isEmpty { lines.append(frame) }
            let values = spectralValues(w)
            if let f = values.frequency { lines.append(Self.frequencyText(f)) }
            if let l = values.wavelength { lines.append(Self.wavelengthText(l)) }
            if let v = values.velocity { lines.append(String(format: "%.4f km/s", v / 1000)) }
            if values.frequency == nil, values.wavelength == nil, values.velocity == nil {
                lines.append(String(format: "%.6g", w) + (cunit.isEmpty ? "" : " " + cunit))
            }
            return lines
        }
    }

    /// The single most useful value for a channel (for the collapsed animator).
    func summary(at index: Int) -> String {
        let lines = info(at: index)
        if kind == .spectral, !frame.isEmpty, lines.count > 1 { return lines[1] }
        return lines.first ?? ""
    }

    /// Frequency (Hz), velocity (m/s) and wavelength (m) of a spectral world value, where known.
    /// Velocities use the radio definition for frequency axes, like CARTA.
    private func spectralValues(_ w: Double) -> (frequency: Double?, velocity: Double?, wavelength: Double?) {
        let c = Self.speedOfLight
        let unit = cunit.lowercased().replacingOccurrences(of: " ", with: "")
        switch String(ctype.prefix(4)) {
        case "FREQ":
            let f = w * Self.frequencyScale(unit)
            return (f, restFrequency.map { c * (1 - f / $0) }, nil)
        case "VRAD", "VOPT", "VELO", "FELO":
            let v = w * Self.velocityScale(unit)
            let prefix = String(ctype.prefix(4))
            let radio = prefix == "VRAD" || (prefix == "VELO" && !(1...255).contains(velref))
            let f = restFrequency.map { radio ? $0 * (1 - v / c) : $0 / (1 + v / c) }
            return (f, v, nil)
        case "WAVE", "AWAV":
            let l = w * Self.lengthScale(unit)
            return (nil, restWavelength.map { c * (l / $0 - 1) }, l)
        default:
            return (nil, nil, nil)
        }
    }

    private static func frequencyScale(_ unit: String) -> Double {
        switch unit {
        case "khz": 1e3
        case "mhz": 1e6
        case "ghz": 1e9
        default: 1          // Hz (the FITS default)
        }
    }

    private static func velocityScale(_ unit: String) -> Double {
        switch unit {
        case "km/s", "kms-1", "km.s-1", "km/sec": 1000
        default: 1          // m/s (the FITS default)
        }
    }

    private static func lengthScale(_ unit: String) -> Double {
        switch unit {
        case "cm": 1e-2
        case "mm": 1e-3
        case "um", "micron", "microns", "µm", "μm": 1e-6
        case "nm": 1e-9
        case "angstrom", "angstroms", "a", "Å": 1e-10
        default: 1          // m (the FITS default)
        }
    }

    static func frequencyText(_ f: Double) -> String {
        let a = abs(f)
        if a >= 1e9 { return String(format: "%.4f GHz", f / 1e9) }
        if a >= 1e6 { return String(format: "%.4f MHz", f / 1e6) }
        if a >= 1e3 { return String(format: "%.4f kHz", f / 1e3) }
        return String(format: "%.4f Hz", f)
    }

    static func wavelengthText(_ l: Double) -> String {
        let a = abs(l)
        if a < 1e-6 { return String(format: "%.4f nm", l / 1e-9) }
        if a < 1e-3 { return String(format: "%.5f µm", l / 1e-6) }
        if a < 1 { return String(format: "%.4f mm", l / 1e-3) }
        return String(format: "%.4f m", l)
    }
}
