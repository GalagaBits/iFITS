//
//  WCS.swift
//  iFITS Start
//
//  World Coordinate System: TAN projection, pixel <-> sky.
//

import Foundation

/// Converts between FITS pixel coordinates (1-based) and world coordinates (degrees).
/// Reads the CD matrix, CDELT + PC, or old-style CROTA2 keywords.
/// RA/Dec axes use the gnomonic (TAN) projection, which almost all imaging and IFU data
/// (JWST, HST, most ground-based cameras) use. Anything else is treated as linear.
struct WCS {
    let crpix1, crpix2: Double
    let crval1, crval2: Double
    let cd11, cd12, cd21, cd22: Double
    let inv11, inv12, inv21, inv22: Double
    let lonpole: Double
    let isCelestial: Bool

    private static let r2d = 180.0 / Double.pi
    private static let d2r = Double.pi / 180.0

    init(header h: [String: String]) {
        func num(_ key: String) -> Double? {
            guard let raw = h[key] else { return nil }
            return Double(raw.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: "D", with: "E"))
        }

        crpix1 = num("CRPIX1") ?? 0
        crpix2 = num("CRPIX2") ?? 0
        crval1 = num("CRVAL1") ?? 0
        crval2 = num("CRVAL2") ?? 0
        lonpole = num("LONPOLE") ?? 180

        let ctype1 = (h["CTYPE1"] ?? "").uppercased()
        let ctype2 = (h["CTYPE2"] ?? "").uppercased()
        isCelestial = ctype1.hasPrefix("RA") && ctype2.hasPrefix("DEC")

        var m: (Double, Double, Double, Double)
        if ["CD1_1", "CD1_2", "CD2_1", "CD2_2"].contains(where: { h[$0] != nil }) {
            m = (num("CD1_1") ?? 0, num("CD1_2") ?? 0, num("CD2_1") ?? 0, num("CD2_2") ?? 0)
        } else {
            let cdelt1 = num("CDELT1") ?? 1
            let cdelt2 = num("CDELT2") ?? 1
            if ["PC1_1", "PC1_2", "PC2_1", "PC2_2"].contains(where: { h[$0] != nil }) {
                m = (cdelt1 * (num("PC1_1") ?? 1), cdelt1 * (num("PC1_2") ?? 0),
                     cdelt2 * (num("PC2_1") ?? 0), cdelt2 * (num("PC2_2") ?? 1))
            } else {
                let rho = (num("CROTA2") ?? 0) * WCS.d2r
                m = (cdelt1 * cos(rho), -cdelt2 * sin(rho),
                     cdelt1 * sin(rho),  cdelt2 * cos(rho))
            }
        }

        var det = m.0 * m.3 - m.1 * m.2
        if abs(det) < 1e-30 { m = (1, 0, 0, 1); det = 1 }
        cd11 = m.0; cd12 = m.1; cd21 = m.2; cd22 = m.3
        inv11 =  m.3 / det; inv12 = -m.1 / det
        inv21 = -m.2 / det; inv22 =  m.0 / det
    }

    static func wrap180(_ x: Double) -> Double {
        var v = x.truncatingRemainder(dividingBy: 360)
        if v > 180 { v -= 360 } else if v < -180 { v += 360 }
        return v
    }

    /// FITS pixel (1-based) → world (RA, Dec in degrees, or linear values).
    func pixelToWorld(_ px: Double, _ py: Double) -> (Double, Double) {
        let dx = px - crpix1, dy = py - crpix2
        let x = cd11 * dx + cd12 * dy
        let y = cd21 * dx + cd22 * dy
        guard isCelestial else { return (crval1 + x, crval2 + y) }

        // Gnomonic (TAN) de-projection, then rotate to celestial coordinates.
        let r = hypot(x, y)
        let phi = atan2(x, -y)
        let theta = atan2(WCS.r2d, r)
        let ap = crval1 * WCS.d2r, dp = crval2 * WCS.d2r
        let dphi = phi - lonpole * WCS.d2r
        let sinT = sin(theta), cosT = cos(theta)

        let dec = asin(max(-1, min(1, sinT * sin(dp) + cosT * cos(dp) * cos(dphi))))
        let ra = ap + atan2(-cosT * sin(dphi), sinT * cos(dp) - cosT * sin(dp) * cos(dphi))
        var raDeg = (ra * WCS.r2d).truncatingRemainder(dividingBy: 360)
        if raDeg < 0 { raDeg += 360 }
        return (raDeg, dec * WCS.r2d)
    }

    /// World → FITS pixel (1-based). Returns nil for points on the far side of the sky.
    func worldToPixel(_ w1: Double, _ w2: Double) -> (Double, Double)? {
        let x: Double, y: Double
        if isCelestial {
            let a = w1 * WCS.d2r, d = w2 * WCS.d2r
            let ap = crval1 * WCS.d2r, dp = crval2 * WCS.d2r
            let da = a - ap
            let sinTheta = sin(d) * sin(dp) + cos(d) * cos(dp) * cos(da)
            guard sinTheta > 1e-6 else { return nil }
            let theta = asin(min(1, sinTheta))
            let phi = lonpole * WCS.d2r
                + atan2(-cos(d) * sin(da), sin(d) * cos(dp) - cos(d) * sin(dp) * cos(da))
            let r = WCS.r2d * cos(theta) / sin(theta)
            x = r * sin(phi)
            y = -r * cos(phi)
        } else {
            x = w1 - crval1
            y = w2 - crval2
        }
        return (crpix1 + inv11 * x + inv12 * y,
                crpix2 + inv21 * x + inv22 * y)
    }
}
