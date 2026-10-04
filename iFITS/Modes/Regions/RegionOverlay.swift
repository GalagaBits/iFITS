//
//  RegionOverlay.swift
//  iFITS Start
//
//  Draws the regions over the image.
//

import SwiftUI

/// Draws every region on top of the image, at screen resolution (lines stay sharp at any zoom).
/// The selected region gets a name tag, plus resize / rotate handles in R mode.
struct RegionOverlay: View {
    let regions: [FITSRegion]
    let selectedID: UUID?
    let showHandles: Bool
    let imageToScreen: CGAffineTransform
    let imageHeight: CGFloat

    var body: some View {
        Canvas { context, _ in
            for region in regions where region.id != selectedID {
                draw(region, selected: false, in: context)
            }
            if let selected = regions.first(where: { $0.id == selectedID }) {
                draw(selected, selected: true, in: context)
            }
        }
        .allowsHitTesting(false)
    }

    private func draw(_ region: FITSRegion, selected: Bool, in context: GraphicsContext) {
        let layout = RegionLayout(region, imageToScreen: imageToScreen, imageHeight: imageHeight)
        let color = Color(regionHex: region.colorHex)
        let path = layout.path

        // A faint dark edge keeps thin colored lines visible on bright parts of the image.
        context.stroke(path, with: .color(.black.opacity(0.45)), lineWidth: selected ? 4.5 : 3.5)
        context.stroke(path, with: .color(color), lineWidth: selected ? 2.5 : 1.5)
        if region.shape == .point {
            let c = layout.center
            context.fill(Path(ellipseIn: CGRect(x: c.x - 1.5, y: c.y - 1.5, width: 3, height: 3)),
                         with: .color(color))
        }
        guard selected else { return }

        if showHandles {
            switch region.shape {
            case .ellipse, .rectangle:
                if region.shape == .ellipse {
                    context.stroke(layout.boxPath, with: .color(color.opacity(0.6)),
                                   style: StrokeStyle(lineWidth: 1, dash: [4, 3]))
                }
                var stem = Path()
                stem.move(to: layout.topMiddle)
                stem.addLine(to: layout.rotationHandle)
                context.stroke(stem, with: .color(color), lineWidth: 1.5)
                roundHandle(at: layout.rotationHandle, color: color, in: context)
                for handle in layout.handles {
                    squareHandle(at: handle.point, color: color, in: context)
                }
            case .line:
                for end in layout.lineEnds {
                    squareHandle(at: end, color: color, in: context)
                }
            case .point:
                break
            }
        }

        // Name tag just below the region.
        let text = context.resolve(Text(region.name)
            .font(.caption.weight(.semibold))
            .foregroundColor(color))
        let size = text.measure(in: CGSize(width: 400, height: 60))
        let anchor = layout.labelAnchor
        let rect = CGRect(x: anchor.x - size.width / 2 - 5, y: anchor.y + 4,
                          width: size.width + 10, height: size.height + 4)
        context.fill(Path(roundedRect: rect, cornerRadius: 5), with: .color(.black.opacity(0.6)))
        context.draw(text, at: CGPoint(x: rect.midX, y: rect.midY), anchor: .center)
    }

    private func squareHandle(at p: CGPoint, color: Color, in context: GraphicsContext) {
        let rect = Path(CGRect(x: p.x - 4.5, y: p.y - 4.5, width: 9, height: 9))
        context.fill(rect, with: .color(.white))
        context.stroke(rect, with: .color(color), lineWidth: 1.5)
    }

    private func roundHandle(at p: CGPoint, color: Color, in context: GraphicsContext) {
        let circle = Path(ellipseIn: CGRect(x: p.x - 5.5, y: p.y - 5.5, width: 11, height: 11))
        context.fill(circle, with: .color(.white))
        context.stroke(circle, with: .color(color), lineWidth: 1.5)
    }
}
