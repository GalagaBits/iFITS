//
//  AnimatorViews.swift
//  iFITS Start
//
//  The animator panel, transport buttons and the mini animator pill.
//

import SwiftUI

/// First / previous / play-pause / next / last.
struct AnimatorTransport: View {
    let animator: CubeAnimator
    var compact = false

    var body: some View {
        HStack(spacing: compact ? 0 : 2) {
            button("backward.end.fill", "First channel") { animator.first() }
            button("backward.frame.fill", "Previous channel") { animator.step(-1) }
            button(animator.isPlaying ? "pause.fill" : "play.fill", animator.isPlaying ? "Pause" : "Play") {
                animator.togglePlay()
            }
            button("forward.frame.fill", "Next channel") { animator.step(1) }
            button("forward.end.fill", "Last channel") { animator.last() }
        }
    }

    private func button(_ symbol: String, _ label: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: symbol)
                .font(compact ? .body : .title3)
                .contentTransition(.symbolEffect(.replace))
                .frame(width: compact ? 38 : 48, height: compact ? 34 : 40)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityLabel(label)
    }
}

/// C mode: the animator in the bottom dock (CARTA-style).
struct AnimatorPanel: View {
    @Bindable var animator: CubeAnimator
    @Binding var expanded: Bool
    let glassNamespace: Namespace.ID

    var body: some View {
        CollapsibleDock(expanded: $expanded, glassNamespace: glassNamespace) {
            VStack(alignment: .leading, spacing: 10) {
                HStack(spacing: 10) {
                    CubeModeIcon()
                        .frame(width: 20, height: 20)
                    Text("Animator")
                        .font(.headline)
                    Spacer()
                    DockCollapseButton(expanded: $expanded)
                }
                controls
                Divider()
                ForEach(animator.steppableAxes, id: \.self) { axis in
                    axisRow(axis)
                }
            }
        } mini: {
            HStack(spacing: 10) {
                CubeModeIcon()
                    .frame(width: 18, height: 18)
                if let axis = animator.currentAxis {
                    Text("\(axis.name) \(animator.current)")
                        .font(.subheadline.weight(.semibold).monospacedDigit())
                    Text(axis.summary(at: animator.current))
                        .font(.subheadline.monospacedDigit())
                        .foregroundStyle(.secondary)
                }
                Image(systemName: "chevron.up")
                    .font(.caption.weight(.bold))
            }
        }
    }

    private var controls: some View {
        HStack(spacing: 12) {
            AnimatorTransport(animator: animator)
                .padding(.horizontal, 4)
                .background(.thinMaterial, in: Capsule())

            Menu {
                Picker("Playback", selection: $animator.playMode) {
                    ForEach(CubePlayMode.allCases) { mode in
                        Label(mode.title, systemImage: mode.symbol).tag(mode)
                    }
                }
            } label: {
                Image(systemName: animator.playMode.symbol)
                    .font(.body.weight(.semibold))
                    .frame(width: 40, height: 36)
                    .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
                    .contentShape(Rectangle())
            }
            .accessibilityLabel("Playback: \(animator.playMode.title)")

            Spacer(minLength: 8)

            Text("Frame rate")
                .font(.subheadline)
                .foregroundStyle(.secondary)
            Text("\(animator.framesPerSecond) fps")
                .font(.body.monospacedDigit())
                .frame(minWidth: 52, alignment: .trailing)
            Stepper("Frame rate", value: $animator.framesPerSecond, in: 1...60)
                .labelsHidden()
        }
    }

    private func axisRow(_ axis: Int) -> some View {
        let info = animator.axes[axis]
        let index = animator.index(onAxis: axis)
        let isAnimated = animator.currentAxis?.number == info.number
        return HStack(alignment: .top, spacing: 14) {
            Button {
                withAnimation(.snappy) { animator.selectAnimatedAxis(axis) }
            } label: {
                HStack(spacing: 8) {
                    if animator.steppableAxes.count > 1 {
                        Image(systemName: isAnimated ? "largecircle.fill.circle" : "circle")
                            .foregroundStyle(isAnimated ? Color.accentColor : Color.secondary)
                    }
                    Text(info.name)
                        .font(.subheadline.weight(.semibold))
                        .lineLimit(1)
                }
                .frame(width: 100, alignment: .leading)
                .padding(.top, 2)
                .contentShape(Rectangle())
            }
            .buttonStyle(.plain)
            .disabled(animator.steppableAxes.count <= 1)
            .accessibilityLabel("Play along \(info.name)")

            VStack(alignment: .leading, spacing: 4) {
                ChannelSlider(value: Binding(get: { animator.index(onAxis: axis) },
                                             set: { animator.setIndex($0, onAxis: axis) }),
                              count: info.length,
                              label: info.name)
                if isAnimated {
                    ChannelRangeSlider(lower: $animator.rangeLower, upper: $animator.rangeUpper, count: info.length)
                }
            }

            VStack(alignment: .leading, spacing: 1) {
                Text("\(info.name) \(index) / \(info.length - 1)")
                    .fontWeight(.semibold)
                ForEach(Array(info.info(at: index).enumerated()), id: \.offset) { _, line in
                    Text(line)
                }
            }
            .font(.caption.monospacedDigit())
            .lineLimit(1)
            .frame(width: 150, alignment: .leading)
        }
    }
}

/// Bottom-right glass pill with the animator buttons, shown in every mode once the animator
/// is collapsed (until closed with ✕).
struct MiniAnimatorBar: View {
    let animator: CubeAnimator
    var onClose: () -> Void

    var body: some View {
        HStack(spacing: 2) {
            AnimatorTransport(animator: animator, compact: true)
            Text("\(animator.current)")
                .font(.subheadline.weight(.semibold).monospacedDigit())
                .frame(minWidth: 30)
                .accessibilityLabel("\(animator.currentAxis?.name ?? "Channel") \(animator.current)")
            Divider()
                .frame(height: 22)
                .padding(.horizontal, 2)
            Button(action: onClose) {
                Image(systemName: "xmark")
                    .font(.subheadline.weight(.bold))
                    .frame(width: 36, height: 34)
                    .contentShape(Rectangle())
            }
            .buttonStyle(.plain)
            .hoverEffect(.highlight)
            .accessibilityLabel("Close animator")
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .glassEffect(.regular, in: Capsule())
    }
}
