//
//  RegionPointer.swift
//  iFITS Start
//
//  Trackpad / mouse pointer shapes over regions.
//

import SwiftUI
import UIKit

/// What the pointer turns into over a region in R mode.
enum RegionPointer: Equatable {
    /// Over a region: four arrows (drag to move).
    case move
    /// Over the rotation handle: a curved arrow.
    case rotate
    /// A drawing tool is on: a crosshair.
    case crosshair
    /// Over a resize handle: two arrows along the stretch direction (screen degrees, 0..<180).
    case stretch(Int)

    /// Stretch pointer along a screen direction (degrees; 0 = right, 90 = down), in 15° steps.
    static func stretching(alongDegrees degrees: Double) -> RegionPointer {
        var d = degrees.truncatingRemainder(dividingBy: 180)
        if d < 0 { d += 180 }
        return .stretch(Int((d / 15).rounded()) * 15 % 180)
    }

    var identifier: String {
        switch self {
        case .move: "move"
        case .rotate: "rotate"
        case .crosshair: "crosshair"
        case .stretch(let degrees): "stretch-\(degrees)"
        }
    }

    init?(identifier: String) {
        switch identifier {
        case "move": self = .move
        case "rotate": self = .rotate
        case "crosshair": self = .crosshair
        default:
            guard identifier.hasPrefix("stretch-"), let d = Int(identifier.dropFirst(8)) else { return nil }
            self = .stretch(d)
        }
    }

    var style: UIPointerStyle {
        switch self {
        case .move:
            let style = UIPointerStyle(shape: UIPointerShape.path(Self.dot), constrainedAxes: [])
            let arrows: [UIPointerAccessory] = [
                UIPointerAccessory.arrow(UIPointerAccessory.Position.top),
                UIPointerAccessory.arrow(UIPointerAccessory.Position.bottom),
                UIPointerAccessory.arrow(UIPointerAccessory.Position.left),
                UIPointerAccessory.arrow(UIPointerAccessory.Position.right)
            ]
            style.accessories = arrows
            return style
        case .stretch(let degrees):
            let style = UIPointerStyle(shape: UIPointerShape.path(Self.dot), constrainedAxes: [])
            let arrows: [UIPointerAccessory] = [
                UIPointerAccessory.arrow(Self.position(screenDegrees: Double(degrees))),
                UIPointerAccessory.arrow(Self.position(screenDegrees: Double(degrees) + 180))
            ]
            style.accessories = arrows
            return style
        case .rotate:
            return UIPointerStyle(shape: UIPointerShape.path(Self.rotateGlyph), constrainedAxes: [])
        case .crosshair:
            return UIPointerStyle(shape: UIPointerShape.path(Self.crosshairGlyph), constrainedAxes: [])
        }
    }

    private static var dot: UIBezierPath {
        UIBezierPath(ovalIn: CGRect(x: -5, y: -5, width: 10, height: 10))
    }

    /// Accessory position pointing along a screen direction (0° = right, 90° = down).
    private static func position(screenDegrees: Double) -> UIPointerAccessory.Position {
        let top = UIPointerAccessory.Position.top
        let right = UIPointerAccessory.Position.right
        // Learn which way UIKit measures accessory angles from its own presets.
        var turn = Double(right.angle - top.angle).truncatingRemainder(dividingBy: 2 * .pi)
        if turn > .pi { turn -= 2 * .pi } else if turn < -.pi { turn += 2 * .pi }
        let fromTop = (screenDegrees + 90) * .pi / 180          // clockwise from the top
        return UIPointerAccessory.Position(offset: top.offset,
                                           angle: top.angle + CGFloat(turn > 0 ? fromTop : -fromTop))
    }

    /// A circular arrow.
    private static var rotateGlyph: UIBezierPath {
        let r: CGFloat = 8
        let start = -CGFloat.pi / 2 + 0.55
        let end = start + 2 * .pi - 1.25
        let arc = UIBezierPath(arcCenter: .zero, radius: r, startAngle: start, endAngle: end, clockwise: true)
        let glyph = UIBezierPath(cgPath: arc.cgPath.copy(strokingWithWidth: 2.4, lineCap: .round,
                                                          lineJoin: .round, miterLimit: 2))
        // Arrowhead at the end of the arc.
        let tip = CGPoint(x: r * cos(end), y: r * sin(end))
        let along = CGVector(dx: -sin(end), dy: cos(end))
        let out = CGVector(dx: cos(end), dy: sin(end))
        let head = UIBezierPath()
        head.move(to: CGPoint(x: tip.x + along.dx * 5, y: tip.y + along.dy * 5))
        head.addLine(to: CGPoint(x: tip.x + out.dx * 4.5 - along.dx * 1.5, y: tip.y + out.dy * 4.5 - along.dy * 1.5))
        head.addLine(to: CGPoint(x: tip.x - out.dx * 4.5 - along.dx * 1.5, y: tip.y - out.dy * 4.5 - along.dy * 1.5))
        head.close()
        glyph.append(head)
        return glyph
    }

    private static var crosshairGlyph: UIBezierPath {
        let path = UIBezierPath(rect: CGRect(x: -10, y: -1, width: 20, height: 2))
        path.append(UIBezierPath(rect: CGRect(x: -1, y: -10, width: 2, height: 20)))
        return path
    }
}
