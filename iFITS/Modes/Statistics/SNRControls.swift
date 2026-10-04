//
//  SNRControls.swift
//  iFITS Start
//
//  Second page of the S-mode dock: the noise, the SNR cutoff, and "Calculate SNR".
//

import SwiftUI

struct SNRControls: View {
    @Bindable var model: SNRModel
    /// The image's BUNIT (the noise is in the same units).
    let unit: String
    /// The image's NAXIS1, NAXIS2, … (empty if it can't be read for SNR).
    let signalAxes: [Int]
    /// Image HDUs of the open file that could be the noise.
    let fileHDUs: [FITSHDUInfo]
    /// For an "_SNR_" file made by iFITS: what it holds.
    let productInfo: String?
    var onOpenNoiseFile: () -> Void
    var onCalculate: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            if let productInfo {
                Label(productInfo, systemImage: "checkmark.seal")
                    .font(.callout)
                    .foregroundStyle(.secondary)
            }

            Grid(alignment: .leading, horizontalSpacing: 12, verticalSpacing: 10) {
                GridRow {
                    Text("Noise")
                        .foregroundStyle(.secondary)
                    noiseMenu
                }
                if model.kind == .manual {
                    GridRow {
                        Text("Noise value")
                            .foregroundStyle(.secondary)
                        HStack(spacing: 8) {
                            NumberField(value: model.manualSigma) { model.manualSigma = $0 }
                                .frame(width: 150)
                            Text(unit.isEmpty ? "(no BUNIT)" : unit)
                                .foregroundStyle(.secondary)
                            Text("for every pixel and channel")
                                .font(.caption)
                                .foregroundStyle(.secondary)
                        }
                    }
                } else if let match = selectedMatch {
                    GridRow {
                        Color.clear.gridCellUnsizedAxes([.horizontal, .vertical])
                        matchLabel(match)
                            .font(.caption)
                    }
                }
                GridRow {
                    Text("Keep SNR ≥")
                        .foregroundStyle(.secondary)
                    HStack(spacing: 8) {
                        NumberField(value: model.cutoff) { model.cutoff = $0 }
                            .frame(width: 150)
                        Text("σ")
                            .foregroundStyle(.secondary)
                    }
                }
            }
            .font(.callout)

            HStack(spacing: 12) {
                Button(action: onOpenNoiseFile) {
                    Label("Open Noise File…", systemImage: "folder")
                }
                .buttonStyle(.bordered)
                .disabled(model.isCalculating)

                Spacer(minLength: 0)

                if model.isCalculating {
                    ProgressView(value: model.progress)
                        .frame(width: 160)
                    Text("Calculating…")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                } else {
                    Button(action: onCalculate) {
                        Label("Calculate SNR", systemImage: "waveform.path.ecg")
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(!canCalculate)
                }
            }

            if let message = model.message {
                Text(message)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }
            Text("SNR = value / noise for every pixel of every channel. Calculating saves a new file, "
                 + "name_SNR_\(SNRCalculator.cutoffText(model.cutoff)).fits, with pixels below the cutoff set to NaN, "
                 + "and opens it. Statistics then cover only the significant pixels.")
                .font(.caption)
                .foregroundStyle(.tertiary)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    // MARK: Noise menu

    private var noiseMenu: some View {
        Menu {
            Button {
                model.kind = .manual
            } label: {
                checked("Single value" + (unit.isEmpty ? "" : " (\(unit))"), model.kind == .manual)
            }
            if !fileHDUs.isEmpty {
                Section("This file") {
                    ForEach(fileHDUs) { hdu in
                        Button {
                            model.kind = .hdu(hdu.index)
                        } label: {
                            checked("\(hdu.label)  \(hdu.shapeText)", model.kind == .hdu(hdu.index))
                        }
                        .disabled(!NoiseShapeMatch.check(noise: hdu.axes, signal: signalAxes).isUsable)
                    }
                }
            }
            if let file = model.noiseFile {
                Section("Noise file") {
                    Button {
                        model.kind = .file
                    } label: {
                        checked(file.label, model.kind == .file)
                    }
                }
            }
            Divider()
            Button(action: onOpenNoiseFile) {
                Label("Open Noise File…", systemImage: "folder")
            }
        } label: {
            DockMenuLabel(text: currentLabel)
        }
        .menuOrder(.fixed)
        .disabled(model.isCalculating)
    }

    @ViewBuilder
    private func checked(_ text: String, _ isOn: Bool) -> some View {
        if isOn {
            Label(text, systemImage: "checkmark")
        } else {
            Text(text)
        }
    }

    private var currentLabel: String {
        switch model.kind {
        case .manual:
            return "Single value"
        case .hdu(let index):
            return fileHDUs.first { $0.index == index }?.label ?? "HDU \(index)"
        case .file:
            return model.noiseFile?.label ?? "Noise file"
        }
    }

    // MARK: Checks

    /// How the chosen noise HDU fits the image (nil for a single value).
    private var selectedMatch: NoiseShapeMatch? {
        switch model.kind {
        case .manual:
            return nil
        case .hdu(let index):
            guard let hdu = fileHDUs.first(where: { $0.index == index }) else {
                return .mismatch("That HDU isn't available any more. Choose the noise again.")
            }
            return NoiseShapeMatch.check(noise: hdu.axes, signal: signalAxes)
        case .file:
            guard let file = model.noiseFile else { return .mismatch("Open a noise file first.") }
            return NoiseShapeMatch.check(noise: file.reader.axes, signal: signalAxes)
        }
    }

    @ViewBuilder
    private func matchLabel(_ match: NoiseShapeMatch) -> some View {
        switch match {
        case .perChannel:
            Label("Same size as the image: one noise value per pixel and channel", systemImage: "checkmark.circle")
                .foregroundStyle(.green)
        case .spatial:
            Label("2-D noise: the same map is used for every channel", systemImage: "checkmark.circle")
                .foregroundStyle(.green)
        case .mismatch(let why):
            Label(why, systemImage: "exclamationmark.triangle")
                .foregroundStyle(.orange)
        }
    }

    private var canCalculate: Bool {
        guard !model.isCalculating, !signalAxes.isEmpty, model.cutoff.isFinite else { return false }
        if model.kind == .manual {
            return model.manualSigma.isFinite && model.manualSigma > 0
        }
        return selectedMatch?.isUsable ?? false
    }
}
