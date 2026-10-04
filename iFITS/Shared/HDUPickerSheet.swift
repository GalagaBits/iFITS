//
//  HDUPickerSheet.swift
//  iFITS Start
//
//  Pop-up for choosing an image HDU (SCI, ERR, DQ, …): when opening a file with more than one,
//  and when choosing the noise HDU of another file for SNR.
//

import SwiftUI

/// Which file's HDUs to choose from, and why.
struct HDUPickerRequest: Identifiable {
    enum Purpose {
        /// Open this HDU as the image.
        case open
        /// Use this HDU as the noise for SNR. It must fit `requiredAxes`.
        case noise
    }

    let id = UUID()
    let purpose: Purpose
    let url: URL
    /// The file's image HDUs.
    let hdus: [FITSHDUInfo]
    /// For noise: the image's NAXIS1, NAXIS2, … the HDU has to fit.
    var requiredAxes: [Int] = []

    var fileName: String { url.lastPathComponent }
}

struct HDUPickerSheet: View {
    let request: HDUPickerRequest
    var onChoose: (FITSHDUInfo) -> Void
    var onCancel: () -> Void

    @State private var selection: Int?

    private var selected: FITSHDUInfo? {
        request.hdus.first { $0.index == selection }
    }

    var body: some View {
        NavigationStack {
            List {
                Section {
                    ForEach(request.hdus) { hdu in
                        Button {
                            selection = hdu.index
                        } label: {
                            row(hdu)
                        }
                        .buttonStyle(.plain)
                        .disabled(!isUsable(hdu))
                    }
                } header: {
                    Text(request.purpose == .open ? "Choose the image to show" : "Choose the noise image")
                } footer: {
                    if request.purpose == .noise {
                        Text("The noise must have the same width and height as the image ("
                             + NoiseShapeMatch.shape(request.requiredAxes)
                             + "), and either the same channels or just one plane, which is then used for every channel.")
                    }
                }

                if let hdu = selected {
                    Section("File Information") {
                        ForEach(information(hdu), id: \.label) { item in
                            LabeledContent(item.label) {
                                Text(item.value)
                                    .font(.callout.monospaced())
                                    .multilineTextAlignment(.trailing)
                                    .textSelection(.enabled)
                            }
                        }
                    }
                }
            }
            .navigationTitle(request.fileName)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel", action: onCancel)
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button(request.purpose == .open ? "Open" : "Use as Noise") {
                        if let hdu = selected { onChoose(hdu) }
                    }
                    .disabled(selected.map { !isUsable($0) } ?? true)
                }
            }
        }
        .onAppear { selection = defaultSelection }
    }

    // MARK: Rows

    private func row(_ hdu: FITSHDUInfo) -> some View {
        HStack(spacing: 12) {
            VStack(alignment: .leading, spacing: 2) {
                Text(hdu.label)
                    .font(.body.monospaced().weight(.semibold))
                Text(hdu.summary)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                if request.purpose == .noise {
                    matchText(hdu)
                        .font(.caption)
                }
            }
            Spacer(minLength: 8)
            if selection == hdu.index {
                Image(systemName: "checkmark")
                    .font(.body.weight(.semibold))
                    .foregroundStyle(.tint)
            }
        }
        .padding(.vertical, 2)
        .contentShape(Rectangle())
        .opacity(isUsable(hdu) ? 1 : 0.45)
    }

    @ViewBuilder
    private func matchText(_ hdu: FITSHDUInfo) -> some View {
        switch NoiseShapeMatch.check(noise: hdu.axes, signal: request.requiredAxes) {
        case .perChannel:
            Label("Same size: one value per pixel and channel", systemImage: "checkmark.circle")
                .foregroundStyle(.green)
        case .spatial:
            Label("2-D: used for every channel", systemImage: "checkmark.circle")
                .foregroundStyle(.green)
        case .mismatch:
            Label("Different size", systemImage: "xmark.circle")
                .foregroundStyle(.red)
        }
    }

    private func isUsable(_ hdu: FITSHDUInfo) -> Bool {
        request.purpose == .open || NoiseShapeMatch.check(noise: hdu.axes, signal: request.requiredAxes).isUsable
    }

    /// SCI when there is one (e.g. JWST), else the first image; for noise, ERR or the first that fits.
    private var defaultSelection: Int? {
        let usable = request.hdus.filter(isUsable)
        let preferred = request.purpose == .open ? "SCI" : "ERR"
        return (usable.first { $0.name.uppercased() == preferred } ?? usable.first)?.index
    }

    // MARK: File information (like CARTA's file browser)

    private struct Item {
        let label: String
        let value: String
    }

    private func information(_ hdu: FITSHDUInfo) -> [Item] {
        let k = hdu.keys
        func text(_ key: String) -> String? {
            guard let v = k[key]?.trimmingCharacters(in: .whitespaces), !v.isEmpty else { return nil }
            return v
        }
        func number(_ key: String) -> Double? { FITSHDUList.number(k[key]) }
        func g(_ v: Double) -> String { String(format: "%.6g", v) }

        var items: [Item] = [
            Item(label: "Name", value: request.fileName),
            Item(label: "HDU", value: "\(hdu.index)"),
            Item(label: "Extension name", value: hdu.extname.isEmpty ? "—" : hdu.extname),
            Item(label: "Data type", value: hdu.dataTypeText)
        ]
        let axisNames = hdu.axes.indices.map { i -> String in
            let ctype = text("CTYPE\(i + 1)") ?? ""
            return ctype.split(separator: "-").first.map(String.init) ?? ""
        }
        var shape = "[" + hdu.axes.map(String.init).joined(separator: ", ") + "]"
        if axisNames.contains(where: { !$0.isEmpty }) {
            shape += " (" + axisNames.map { $0.isEmpty ? "?" : $0 }.joined(separator: ", ") + ")"
        }
        items.append(Item(label: "Shape", value: shape))
        if hdu.axes.count >= 3 {
            items.append(Item(label: "Number of channels", value: "\(hdu.planeCount)"))
        }
        if let unit = text("BUNIT") {
            items.append(Item(label: "Pixel unit", value: unit))
        }
        if let c1 = text("CTYPE1"), let c2 = text("CTYPE2") {
            let isRADec = c1.uppercased().hasPrefix("RA") && c2.uppercased().hasPrefix("DEC")
            items.append(Item(label: "Coordinate type", value: isRADec ? "Right Ascension, Declination" : "\(c1), \(c2)"))
            if let projection = c1.split(separator: "-", omittingEmptySubsequences: true).dropFirst().first {
                items.append(Item(label: "Projection", value: String(projection)))
            }
        }
        if let p1 = number("CRPIX1"), let p2 = number("CRPIX2") {
            items.append(Item(label: "Reference pixel", value: "[\(g(p1)), \(g(p2))]"))
        }
        if let v1 = number("CRVAL1"), let v2 = number("CRVAL2") {
            items.append(Item(label: "Reference coords", value: "[\(g(v1)), \(g(v2))]"))
        }
        // Pixel size: CDELT, or the length of the CD matrix columns.
        let d1 = number("CDELT1") ?? number("CD1_1").map { a in (a * a + pow(number("CD2_1") ?? 0, 2)).squareRoot() }
        let d2 = number("CDELT2") ?? number("CD2_2").map { b in (b * b + pow(number("CD1_2") ?? 0, 2)).squareRoot() }
        if let d1, let d2 {
            let celestial = (text("CTYPE1") ?? "").uppercased().hasPrefix("RA")
            items.append(Item(label: "Pixel increment",
                              value: celestial ? String(format: "%.4g″, %.4g″", abs(d1) * 3600, abs(d2) * 3600)
                                               : "\(g(d1)), \(g(d2))"))
        }
        if let frame = text("RADESYS") ?? text("RADECSYS") {
            items.append(Item(label: "Celestial frame", value: frame))
        }
        if let c3 = text("CTYPE3") {
            let unit = text("CUNIT3").map { " (\($0))" } ?? ""
            items.append(Item(label: "Spectral axis", value: c3 + unit))
        }
        if let spec = text("SPECSYS") {
            items.append(Item(label: "Spectral frame", value: spec))
        }
        return items
    }
}
