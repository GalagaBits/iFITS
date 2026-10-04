//
//  RegionGeometry.swift
//  iFITS Start
//
//  Region geometry on screen: layout, hit testing, drag / resize / rotate.
//

import SwiftUI

/// A region laid out on screen (points), for drawing and hit-testing.
struct RegionLayout {
    let region: FITSRegion
    let center: CGPoint
    /// Screen directions of the region's own x axis and its "up" (FITS +y) axis.
    let ux: CGVector
    let uy: CGVector
    let halfWidth: CGFloat
    let halfHeight: CGFloat

    /// Distance of the rotation handle above the region's top edge.
    static let rotationHandleGap: CGFloat = 28

    init(_ region: FITSRegion, imageToScreen t: CGAffineTransform, imageHeight: CGFloat) {
        self.region = region
        center = RegionLayout.screenPoint(region.center, t, imageHeight)
        // FITS y points up and screen y points down, so a counter-clockwise angle in the FITS
        // image is counter-clockwise on screen as well.
        let a = CGFloat(region.radians)
        ux = CGVector(dx: cos(a), dy: -sin(a))
        uy = CGVector(dx: -sin(a), dy: -cos(a))
        let k = sqrt(abs(t.a * t.d - t.b * t.c))      // screen points per image pixel
        halfWidth = region.size.width * k / 2
        halfHeight = region.size.height * k / 2
    }

    /// FITS pixel → screen point (the displayed image has FITS row 1 at the bottom).
    static func screenPoint(_ f: CGPoint, _ t: CGAffineTransform, _ imageHeight: CGFloat) -> CGPoint {
        CGPoint(x: f.x - 0.5, y: imageHeight - f.y + 0.5).applying(t)
    }

    /// Screen point → FITS pixel.
    static func fitsPoint(_ s: CGPoint, _ t: CGAffineTransform, _ imageHeight: CGFloat) -> CGPoint {
        let q = s.applying(t.inverted())
        return CGPoint(x: q.x + 0.5, y: imageHeight - q.y + 0.5)
    }

    /// Screen point at (lx, ly) points along the region's own axes (ly = up).
    func point(_ lx: CGFloat, _ ly: CGFloat) -> CGPoint {
        CGPoint(x: center.x + lx * ux.dx + ly * uy.dx,
                y: center.y + lx * ux.dy + ly * uy.dy)
    }

    /// Screen point → position along the region's own axes, in points.
    func local(_ p: CGPoint) -> CGPoint {
        let vx = p.x - center.x, vy = p.y - center.y
        return CGPoint(x: vx * ux.dx + vy * ux.dy, y: vx * uy.dx + vy * uy.dy)
    }

    /// Resize handles of an ellipse or rectangle: the corners and edge midpoints of its box.
    var handles: [(sx: Int, sy: Int, point: CGPoint)] {
        var out: [(sx: Int, sy: Int, point: CGPoint)] = []
        for sy in [1, 0, -1] {
            for sx in [-1, 0, 1] where !(sx == 0 && sy == 0) {
                out.append((sx, sy, point(CGFloat(sx) * halfWidth, CGFloat(sy) * halfHeight)))
            }
        }
        return out
    }

    var topMiddle: CGPoint { point(0, halfHeight) }
    var rotationHandle: CGPoint { point(0, halfHeight + Self.rotationHandleGap) }
    var lineEnds: [CGPoint] { [point(-halfWidth, 0), point(halfWidth, 0)] }

    /// Screen angle (degrees; 0 = right, 90 = down) of the direction a handle stretches.
    func stretchAxisDegrees(sx: Int, sy: Int) -> Double {
        let w = max(halfWidth, 1), h = max(halfHeight, 1)
        let dx = CGFloat(sx) * w * ux.dx + CGFloat(sy) * h * uy.dx
        let dy = CGFloat(sx) * w * ux.dy + CGFloat(sy) * h * uy.dy
        return Double(atan2(dy, dx)) * 180 / .pi
    }

    /// Maps a shape drawn around (0, 0) in local points onto the screen.
    private var localToScreen: CGAffineTransform {
        CGAffineTransform(a: ux.dx, b: ux.dy, c: -uy.dx, d: -uy.dy, tx: center.x, ty: center.y)
    }

    private var localBox: CGRect {
        CGRect(x: -halfWidth, y: -halfHeight, width: halfWidth * 2, height: halfHeight * 2)
    }

    var path: Path {
        switch region.shape {
        case .ellipse:
            return Path(ellipseIn: localBox).applying(localToScreen)
        case .rectangle:
            return Path(localBox).applying(localToScreen)
        case .line:
            var p = Path()
            let ends = lineEnds
            p.move(to: ends[0])
            p.addLine(to: ends[1])
            return p
        case .point:
            return Path(ellipseIn: CGRect(x: center.x - 5.5, y: center.y - 5.5, width: 11, height: 11))
        }
    }

    /// The rotated box around an ellipse (shown while it's being edited).
    var boxPath: Path { Path(localBox).applying(localToScreen) }

    /// Whether a screen point is on or inside the region (with a little slack for fingers).
    func contains(_ p: CGPoint, tolerance: CGFloat = 8) -> Bool {
        switch region.shape {
        case .rectangle:
            let q = local(p)
            return abs(q.x) <= halfWidth + tolerance && abs(q.y) <= halfHeight + tolerance
        case .ellipse:
            let q = local(p)
            let a = halfWidth + tolerance, b = halfHeight + tolerance
            guard a > 0, b > 0 else { return false }
            return (q.x * q.x) / (a * a) + (q.y * q.y) / (b * b) <= 1
        case .line:
            let q = local(p)
            let along = max(-halfWidth, min(halfWidth, q.x))
            return hypot(q.x - along, q.y) <= tolerance + 4
        case .point:
            return hypot(p.x - center.x, p.y - center.y) <= tolerance + 8
        }
    }

    /// Just below the region on screen, where its name goes.
    var labelAnchor: CGPoint {
        switch region.shape {
        case .point:
            return CGPoint(x: center.x, y: center.y + 9)
        case .line:
            let ends = lineEnds
            return CGPoint(x: center.x, y: max(ends[0].y, ends[1].y) + 4)
        case .ellipse:
            let reach = hypot(halfWidth * ux.dy, halfHeight * uy.dy)
            return CGPoint(x: center.x, y: center.y + reach + 4)
        case .rectangle:
            let reach = abs(halfWidth * ux.dy) + abs(halfHeight * uy.dy)
            return CGPoint(x: center.x, y: center.y + reach + 4)
        }
    }
}

/// What's under a screen point.
enum RegionHit: Equatable {
    case body(UUID)
    case resize(UUID, sx: Int, sy: Int)
    case endpoint(UUID, Int)
    case rotate(UUID)

    var regionID: UUID {
        switch self {
        case .body(let id), .resize(let id, _, _), .endpoint(let id, _), .rotate(let id): id
        }
    }

    var isHandle: Bool {
        if case .body = self { return false }
        return true
    }
}

/// A drag that is drawing or editing a region.
struct RegionDrag {
    enum Kind: Equatable {
        case create(start: CGPoint)
        case move(start: CGPoint)
        case resize(sx: Int, sy: Int)
        case endpoint(Int)
        case rotate
    }

    let id: UUID
    let kind: Kind
    /// The region as it was when the drag started.
    let original: FITSRegion

    /// The region after dragging to `f` (FITS pixels).
    func region(draggedTo f: CGPoint) -> FITSRegion {
        var r = original
        let minSize: CGFloat = 0.05
        switch kind {
        case .create(let s):
            switch r.shape {
            case .point:
                r.center = f
            case .line:
                r.setLine(from: s, to: f)
            case .ellipse, .rectangle:
                // Drawn corner to corner.
                r.center = CGPoint(x: (s.x + f.x) / 2, y: (s.y + f.y) / 2)
                r.size = CGSize(width: max(abs(f.x - s.x), minSize), height: max(abs(f.y - s.y), minSize))
            }

        case .move(let s):
            r.center = CGPoint(x: original.center.x + f.x - s.x, y: original.center.y + f.y - s.y)

        case .resize(let sx, let sy):
            // Work along the region's own (rotated) axes; the opposite side stays put.
            let a = original.radians
            let c = CGFloat(cos(a)), sn = CGFloat(sin(a))
            let dx = f.x - original.center.x, dy = f.y - original.center.y
            let lx = dx * c + dy * sn
            let ly = -dx * sn + dy * c
            var w = original.size.width, h = original.size.height
            var cx: CGFloat = 0, cy: CGFloat = 0
            if sx != 0 {
                let side = CGFloat(sx)
                let anchor = -side * w / 2
                w = max(minSize, side * (lx - anchor))
                cx = anchor + side * w / 2
            }
            if sy != 0 {
                let side = CGFloat(sy)
                let anchor = -side * h / 2
                h = max(minSize, side * (ly - anchor))
                cy = anchor + side * h / 2
            }
            r.size = CGSize(width: w, height: h)
            r.center = CGPoint(x: original.center.x + cx * c - cy * sn,
                               y: original.center.y + cx * sn + cy * c)

        case .endpoint(let index):
            let ends = original.lineEnds
            if index == 0 {
                r.setLine(from: f, to: ends.end)
            } else {
                r.setLine(from: ends.start, to: f)
            }

        case .rotate:
            // The rotation handle sits on the region's "up" axis, 90° from its x axis.
            let degrees = Double(atan2(f.y - original.center.y, f.x - original.center.x)) * 180 / .pi
            r.angle = FITSRegion.normalized(degrees - 90)
        }
        return r
    }
}
