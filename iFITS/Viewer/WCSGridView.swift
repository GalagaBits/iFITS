//
//  WCSGridView.swift
//  iFITS Start
//
//  RA / Dec coordinate grid drawn over the image.
//

import SwiftUI

/// Draws the RA/Dec grid in screen space on top of the zoomed image, so lines stay
/// 1 pt sharp and labels stay the same size at any zoom. Line spacing adapts to the
/// zoom level, and labels stick to the bottom and right edges so they're always visible.
struct WCSGridView: View {
    let wcs: WCS
    let imageWidth: CGFloat     // image size in pixels
    let imageHeight: CGFloat
    let scale: CGFloat
    let offset: CGSize

    /// RA as hh:mm:ss and Dec as ±dd:mm:ss. Set to false for decimal degrees.
    var sexagesimal = true
    /// Roughly how many grid lines to show in each direction.
    var targetLineCount: Double = 5
    var lineColor = Color.green.opacity(0.75)
    var labelColor = Color.green
    var labelFont = Font.system(size: 12, weight: .semibold, design: .monospaced)
    /// Keeps labels clear of the toolbar (top), the V/A/R/S buttons (left) and the home indicator (bottom).
    var labelInsets = EdgeInsets(top: 80, leading: 130, bottom: 24, trailing: 12)

    private enum AxisKind { case ra, dec, linear }

    var body: some View {
        Canvas { context, size in
            draw(context, size: size)
        }
    }

    private func draw(_ context: GraphicsContext, size: CGSize) {
        guard imageWidth > 0, imageHeight > 0, size.width > 0, size.height > 0 else { return }

        // Same mapping as the image: aspect-fill, scaled about the center, then offset.
        let baseScale = max(size.width / imageWidth, size.height / imageHeight)
        let px = baseScale * scale      // screen points per image pixel
        let origin = CGPoint(x: size.width / 2 + offset.width - imageWidth * px / 2,
                             y: size.height / 2 + offset.height - imageHeight * px / 2)
        let imageRect = CGRect(x: origin.x, y: origin.y, width: imageWidth * px, height: imageHeight * px)
        let visible = imageRect.intersection(CGRect(origin: .zero, size: size))
        guard !visible.isNull, visible.width > 4, visible.height > 4 else { return }

        // Screen point ↔ FITS pixel (1-based; pixel centers sit on whole numbers).
        // The image is flipped like CARTA (FITS row 1 at the bottom), so FITS y counts upward.
        func toPixel(_ p: CGPoint) -> (Double, Double) {
            (Double((p.x - origin.x) / px) + 0.5,
             Double(imageHeight) - Double((p.y - origin.y) / px) + 0.5)
        }
        func toScreen(_ p: (Double, Double)) -> CGPoint {
            CGPoint(x: origin.x + CGFloat(p.0 - 0.5) * px,
                    y: origin.y + (imageHeight + 0.5 - CGFloat(p.1)) * px)
        }

        // 1. Find the range of world coordinates currently on screen.
        let center = toPixel(CGPoint(x: visible.midX, y: visible.midY))
        let ref1 = wcs.pixelToWorld(center.0, center.1).0
        var min1 = Double.infinity, max1 = -Double.infinity
        var min2 = Double.infinity, max2 = -Double.infinity
        let n = 12
        for i in 0...n {
            for j in 0...n {
                let p = CGPoint(x: visible.minX + visible.width * CGFloat(i) / CGFloat(n),
                                y: visible.minY + visible.height * CGFloat(j) / CGFloat(n))
                let pix = toPixel(p)
                var (w1, w2) = wcs.pixelToWorld(pix.0, pix.1)
                if wcs.isCelestial { w1 = ref1 + WCS.wrap180(w1 - ref1) }   // handle RA 0/360 wrap
                min1 = min(min1, w1); max1 = max(max1, w1)
                min2 = min(min2, w2); max2 = max(max2, w2)
            }
        }
        if wcs.isCelestial {
            // A celestial pole on screen means every RA line passes through it.
            for pole in [90.0, -90.0] {
                if let pp = wcs.worldToPixel(0, pole), visible.contains(toScreen(pp)) {
                    min1 = ref1 - 180; max1 = ref1 + 180
                    if pole > 0 { max2 = 90 } else { min2 = -90 }
                }
            }
        }
        guard max1 > min1, max2 > min2, min1.isFinite, max2.isFinite else { return }

        // 2. Pick "nice" spacings for the current zoom.
        let kind1: AxisKind = wcs.isCelestial ? .ra : .linear
        let kind2: AxisKind = wcs.isCelestial ? .dec : .linear
        let step1 = niceStep(span: max1 - min1, kind: kind1)
        let step2 = niceStep(span: max2 - min2, kind: kind2)

        // 3. Label layout.
        let safe = CGRect(x: labelInsets.leading, y: labelInsets.top,
                          width: size.width - labelInsets.leading - labelInsets.trailing,
                          height: size.height - labelInsets.top - labelInsets.bottom)
        let labelArea = visible.intersection(safe)
        let canLabel = !labelArea.isNull && labelArea.width > 60 && labelArea.height > 30
        let labelHeight = context.resolve(Text("0").font(labelFont))
            .measure(in: CGSize(width: 1000, height: 200)).height + 4
        var placed: [CGRect] = []

        enum Edge { case bottom, right }

        func crossing(_ pts: [CGPoint?], edge: Edge) -> CGPoint? {
            let target = edge == .bottom ? labelArea.maxY - labelHeight / 2 : labelArea.maxX - 4
            for i in 1..<pts.count {
                guard let a = pts[i - 1], let b = pts[i] else { continue }
                let va = edge == .bottom ? a.y : a.x
                let vb = edge == .bottom ? b.y : b.x
                guard va != vb, (va - target) * (vb - target) <= 0 else { continue }
                let t = (target - va) / (vb - va)
                let hit = CGPoint(x: a.x + t * (b.x - a.x), y: a.y + t * (b.y - a.y))
                let inside = edge == .bottom
                    ? (hit.x >= labelArea.minX && hit.x <= labelArea.maxX)
                    : (hit.y >= labelArea.minY && hit.y <= labelArea.maxY)
                if inside { return hit }
            }
            return nil
        }

        func drawLabel(_ string: String, along pts: [CGPoint?], preferred: Edge) {
            guard canLabel else { return }
            let edges: [Edge] = preferred == .bottom ? [.bottom, .right] : [.right, .bottom]
            let resolved = context.resolve(Text(string).font(labelFont).foregroundColor(labelColor))
            let textSize = resolved.measure(in: CGSize(width: 1000, height: 200))
            let w = textSize.width + 10, h = textSize.height + 4

            for edge in edges {
                guard let hit = crossing(pts, edge: edge) else { continue }
                var rect = edge == .bottom
                    ? CGRect(x: hit.x - w / 2, y: labelArea.maxY - h, width: w, height: h)
                    : CGRect(x: labelArea.maxX - w, y: hit.y - h / 2, width: w, height: h)
                rect.origin.x = min(max(rect.minX, labelArea.minX), labelArea.maxX - w)
                rect.origin.y = min(max(rect.minY, labelArea.minY), labelArea.maxY - h)
                if placed.contains(where: { $0.insetBy(dx: -4, dy: -2).intersects(rect) }) { continue }
                placed.append(rect)
                context.fill(Path(roundedRect: rect, cornerRadius: 5), with: .color(.black.opacity(0.6)))
                context.draw(resolved, at: CGPoint(x: rect.midX, y: rect.midY), anchor: .center)
                return
            }
        }

        // 4. Draw lines (clipped to the visible image), then labels on top.
        var lines = context
        lines.clip(to: Path(visible))
        let segments = 64

        func stroke(_ pts: [CGPoint?]) {
            var path = Path()
            var penDown = false
            for p in pts {
                if let p {
                    if penDown { path.addLine(to: p) } else { path.move(to: p); penDown = true }
                } else {
                    penDown = false
                }
            }
            lines.stroke(path, with: .color(lineColor), lineWidth: 1)
        }

        // Lines of constant RA (or axis 1)
        let start1 = (min1 / step1).rounded(.down) * step1
        let count1 = min(200, Int(((max1 - start1) / step1).rounded(.up)))
        var lines1: [(String, [CGPoint?])] = []
        for i in 0...count1 {
            let v = start1 + Double(i) * step1
            let lo = min2 - step2, hi = max2 + step2
            var pts: [CGPoint?] = []
            for k in 0...segments {
                var w2 = lo + (hi - lo) * Double(k) / Double(segments)
                if wcs.isCelestial { w2 = min(90, max(-90, w2)) }
                pts.append(wcs.worldToPixel(v, w2).map { toScreen($0) })
            }
            stroke(pts)
            lines1.append((format(v, kind: kind1, step: step1), pts))
        }

        // Lines of constant Dec (or axis 2)
        let start2 = (min2 / step2).rounded(.down) * step2
        let count2 = min(200, Int(((max2 - start2) / step2).rounded(.up)))
        var lines2: [(String, [CGPoint?])] = []
        for i in 0...count2 {
            let v = start2 + Double(i) * step2
            if wcs.isCelestial && abs(v) >= 90 { continue }
            let lo = min1 - step1, hi = max1 + step1
            var pts: [CGPoint?] = []
            for k in 0...segments {
                let w1 = lo + (hi - lo) * Double(k) / Double(segments)
                pts.append(wcs.worldToPixel(w1, v).map { toScreen($0) })
            }
            stroke(pts)
            lines2.append((format(v, kind: kind2, step: step2), pts))
        }

        for (text, pts) in lines2 { drawLabel(text, along: pts, preferred: .right) }
        for (text, pts) in lines1 { drawLabel(text, along: pts, preferred: .bottom) }
    }

    // MARK: Spacing

    private static let timeSteps: [Double] = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900,
                                              1200, 1800, 3600, 7200, 10800, 21600]   // seconds of time
    private static let arcSteps: [Double] = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900,
                                             1200, 1800, 3600, 7200, 18000, 36000]    // arcseconds

    private static func decimalStep(_ x: Double) -> Double {
        guard x > 0, x.isFinite else { return 1 }
        let e = pow(10, floor(log10(x)))
        let f = x / e
        return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * e
    }

    /// Units per degree for an axis (seconds of time for RA, arcseconds for Dec).
    private func unitsPerDegree(_ kind: AxisKind) -> Double {
        guard sexagesimal else { return 1 }
        switch kind {
        case .ra: return 240
        case .dec: return 3600
        case .linear: return 1
        }
    }

    private func niceStep(span: Double, kind: AxisKind) -> Double {
        let unit = unitsPerDegree(kind)
        let t = span / targetLineCount * unit
        let candidates: [Double]
        switch (kind, sexagesimal) {
        case (.ra, true): candidates = Self.timeSteps
        case (.dec, true): candidates = Self.arcSteps
        default: candidates = []
        }
        if let first = candidates.first, let last = candidates.last, t >= first, t <= last,
           let c = candidates.first(where: { $0 >= t }) {
            return c / unit
        }
        return Self.decimalStep(t) / unit
    }

    // MARK: Labels

    private func decimals(forStep stepUnits: Double) -> Int {
        guard stepUnits > 0 else { return 0 }
        return min(6, max(0, Int(ceil(-log10(stepUnits) - 1e-9))))
    }

    private func format(_ value: Double, kind: AxisKind, step: Double) -> String {
        let unit = unitsPerDegree(kind)
        let d = decimals(forStep: step * unit)

        switch kind {
        case .ra:
            var ra = value.truncatingRemainder(dividingBy: 360)
            if ra < 0 { ra += 360 }
            guard sexagesimal else { return String(format: "%.\(d)f°", ra) }
            let (a, b, s) = sexagesimalParts(ra * 240, decimals: d)
            return String(format: "%02ld:%02ld:", a % 24, b) + s
        case .dec:
            guard sexagesimal else { return String(format: "%+.\(d)f°", value) }
            let (a, b, s) = sexagesimalParts(abs(value) * 3600, decimals: d)
            let isZero = a == 0 && b == 0 && Double(s) == 0
            let sign = (value < 0 && !isZero) ? "-" : "+"
            return sign + String(format: "%02ld:%02ld:", a, b) + s
        case .linear:
            return String(format: "%.\(d)f", value)
        }
    }

    /// Splits a count of seconds into (hours/degrees, minutes, "seconds" string), using
    /// integer math so values never round up to "60".
    private func sexagesimalParts(_ totalSeconds: Double, decimals d: Int) -> (Int, Int, String) {
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
