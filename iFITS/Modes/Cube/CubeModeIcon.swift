//
//  CubeModeIcon.swift
//  iFITS Start
//
//  The cube-with-a-sheet icon for the C button.
//

import SwiftUI

/// A cube with a channel (sheet) sticking out of its top.
struct CubeModeIcon: View {
    var lineWidth: CGFloat = 1.6

    var body: some View {
        ZStack {
            CubeOutlineShape()
                .stroke(style: StrokeStyle(lineWidth: lineWidth, lineCap: .round, lineJoin: .round))
            CubeSheetShape()
                .fill(.foreground)
        }
        .aspectRatio(1, contentMode: .fit)
        .accessibilityHidden(true)
    }
}

/// The cube's visible edges (drawn in a 24 × 24 box; edges hidden by the sheet are left out).
private struct CubeOutlineShape: Shape {
    func path(in rect: CGRect) -> Path {
        let s = min(rect.width, rect.height) / 24
        func p(_ x: CGFloat, _ y: CGFloat) -> CGPoint { CGPoint(x: rect.minX + x * s, y: rect.minY + y * s) }
        var path = Path()
        // Front face
        path.addLines([p(3, 11), p(15, 11), p(15, 23), p(3, 23), p(3, 11)])
        // Top face (its back half is behind the sheet)
        path.move(to: p(3, 11)); path.addLine(to: p(5.5, 8.5))
        path.move(to: p(15, 11)); path.addLine(to: p(20, 6))
        path.move(to: p(17.5, 6)); path.addLine(to: p(20, 6))
        // Right face
        path.move(to: p(20, 6)); path.addLine(to: p(20, 18)); path.addLine(to: p(15, 23))
        return path
    }
}

/// A channel halfway back in the cube, poking out through the top.
private struct CubeSheetShape: Shape {
    func path(in rect: CGRect) -> Path {
        let s = min(rect.width, rect.height) / 24
        return Path(roundedRect: CGRect(x: rect.minX + 5.5 * s, y: rect.minY + 1 * s,
                                        width: 12 * s, height: 7.5 * s),
                    cornerRadius: 1.2 * s)
    }
}
