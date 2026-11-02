//
//  CubeAnimator.swift
//  iFITS Start
//
//  Cube playback: current channel, range, frame rate, play mode.
//

import SwiftUI

enum CubePlayMode: String, CaseIterable, Identifiable {
    case forward, backward, bouncing, blink

    var id: String { rawValue }

    var title: String {
        switch self {
        case .forward: "Forward"
        case .backward: "Backward"
        case .bouncing: "Bouncing"
        case .blink: "Blink (first ↔ last)"
        }
    }

    var symbol: String {
        switch self {
        case .forward: "arrow.right"
        case .backward: "arrow.left"
        case .bouncing: "arrow.left.arrow.right"
        case .blink: "eye"
        }
    }
}

/// Playback state: the channel shown on each axis, the playback range, play / pause,
/// frame rate and playback mode.
@MainActor
@Observable
final class CubeAnimator {
    private(set) var axes: [CubeAxis] = []
    /// Channel shown on each axis (0-based, same order as `axes`).
    var indices: [Int] = []
    /// The axis that plays and steps (index into `axes`).
    private(set) var animatedAxis = 0
    /// Playback range on the animated axis.
    var rangeLower = 0
    var rangeUpper = 0
    var isPlaying = false
    var framesPerSecond = 5
    var playMode: CubePlayMode = .forward
    @ObservationIgnored private var bounceStep = 1

    /// Axes with more than one channel (the ones with sliders).
    var steppableAxes: [Int] { axes.indices.filter { axes[$0].length > 1 } }

    var currentAxis: CubeAxis? { axes.indices.contains(animatedAxis) ? axes[animatedAxis] : nil }
    var current: Int { index(onAxis: animatedAxis) }

    func index(onAxis axis: Int) -> Int {
        indices.indices.contains(axis) ? indices[axis] : 0
    }

    func configure(_ newAxes: [CubeAxis]) {
        isPlaying = false
        axes = newAxes
        indices = Array(repeating: 0, count: newAxes.count)
        selectAnimatedAxis(steppableAxes.first ?? 0)
    }

    /// Picks the axis that plays (the radio buttons), and resets its range to all channels.
    func selectAnimatedAxis(_ axis: Int) {
        animatedAxis = axis
        rangeLower = 0
        rangeUpper = axes.indices.contains(axis) ? axes[axis].length - 1 : 0
        bounceStep = 1
    }

    func setIndex(_ value: Int, onAxis axis: Int) {
        guard indices.indices.contains(axis), axes.indices.contains(axis) else { return }
        let v = min(max(value, 0), axes[axis].length - 1)
        if indices[axis] != v { indices[axis] = v }
    }

    /// Next / previous channel within the playback range (wraps around).
    func step(_ delta: Int) {
        guard currentAxis != nil else { return }
        var v = current + delta
        if v > rangeUpper { v = rangeLower } else if v < rangeLower { v = rangeUpper }
        setIndex(v, onAxis: animatedAxis)
    }

    func first() { setIndex(rangeLower, onAxis: animatedAxis) }
    func last() { setIndex(rangeUpper, onAxis: animatedAxis) }

    func togglePlay() {
        guard !steppableAxes.isEmpty else { return }
        isPlaying.toggle()
    }

    /// The channels the next `count` playback frames will show, in order, without changing
    /// anything: the same steps as `tick()` (forward, backward, bouncing or blink, within the
    /// playback range). Used to load channels ahead of time, like a video player's buffer.
    func upcomingIndices(_ count: Int) -> [[Int]] {
        let lo = rangeLower, hi = rangeUpper
        guard count > 0, currentAxis != nil, hi > lo, indices.indices.contains(animatedAxis) else { return [] }
        var c = current, bounce = bounceStep
        var out: [[Int]] = []
        for _ in 0..<count {
            if c < lo || c > hi {
                c = lo
            } else {
                switch playMode {
                case .forward: c = c + 1 > hi ? lo : c + 1
                case .backward: c = c - 1 < lo ? hi : c - 1
                case .bouncing:
                    if c + bounce > hi || c + bounce < lo { bounce = -bounce }
                    c += bounce
                case .blink: c = c == lo ? hi : lo
                }
            }
            var frame = indices
            frame[animatedAxis] = c
            out.append(frame)
        }
        return out
    }

    /// One playback frame.
    func tick() {
        let lo = rangeLower, hi = rangeUpper
        guard hi > lo else { return }
        let c = current
        if c < lo || c > hi {
            setIndex(lo, onAxis: animatedAxis)
            return
        }
        switch playMode {
        case .forward:
            step(1)
        case .backward:
            step(-1)
        case .bouncing:
            if c + bounceStep > hi || c + bounceStep < lo { bounceStep = -bounceStep }
            setIndex(c + bounceStep, onAxis: animatedAxis)
        case .blink:
            setIndex(c == lo ? hi : lo, onAxis: animatedAxis)
        }
    }
}
