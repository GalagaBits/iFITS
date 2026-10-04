//
//  HeaderWindow.swift
//  iFITS Start
//
//  The FITS header window.
//

import SwiftUI

/// What the header window shows. Passed to the new window when "H" is tapped.
struct FITSHeaderDocument: Codable, Hashable, Identifiable {
    var id: UUID
    var fileName: String
    var cards: [FITSHeaderCard]
}

/// One header card laid out for a narrow window: keyword and value on the first line,
/// the comment underneath.
struct CompactHeaderRow: View {
    let card: FITSHeaderCard

    var body: some View {
        VStack(alignment: .leading, spacing: 3) {
            HStack(alignment: .firstTextBaseline, spacing: 12) {
                Text(card.keyword)
                    .font(.system(.body, design: .monospaced).weight(.semibold))
                    .layoutPriority(1)
                Spacer(minLength: 0)
                if !card.value.isEmpty {
                    Text(card.value)
                        .font(.system(.body, design: .monospaced))
                        .multilineTextAlignment(.trailing)
                        .textSelection(.enabled)
                }
            }
            if !card.comment.isEmpty {
                Text(card.comment)
                    .font(.footnote)
                    .foregroundStyle(.secondary)
                    .textSelection(.enabled)
            }
        }
        .padding(.vertical, 2)
    }
}

/// Header table in its own window: ✕ (top left) closes it, the share button (top right)
/// shares the header as a .txt (FITS card layout) or .csv file.
struct HeaderWindowView: View {
    let document: FITSHeaderDocument
    /// Set when shown as a sheet instead of a window.
    var onClose: (() -> Void)? = nil

    @Environment(\.dismissWindow) private var dismissWindow
    @Environment(\.horizontalSizeClass) private var horizontalSizeClass
    @State private var searchText = ""
    @State private var exportFiles: (txt: URL, csv: URL)?

    private var rows: [FITSHeaderCard] {
        let query = searchText.trimmingCharacters(in: .whitespaces)
        guard !query.isEmpty else { return document.cards }
        return document.cards.filter {
            $0.keyword.localizedCaseInsensitiveContains(query)
                || $0.value.localizedCaseInsensitiveContains(query)
                || $0.comment.localizedCaseInsensitiveContains(query)
        }
    }

    var body: some View {
        NavigationStack {
            Table(rows) {
                // A narrow window ("iPhone size") only shows the first column of a Table,
                // so there the keyword, value and comment are stacked in this one column.
                TableColumn("Keyword") { card in
                    if horizontalSizeClass == .compact {
                        CompactHeaderRow(card: card)
                    } else {
                        Text(card.keyword)
                            .font(.system(.body, design: .monospaced).weight(.semibold))
                    }
                }
                .width(min: 90, ideal: 110)

                TableColumn("Value") { card in
                    Text(card.value)
                        .font(.system(.body, design: .monospaced))
                        .textSelection(.enabled)
                }
                .width(min: 120, ideal: 240)

                TableColumn("Comment") { card in
                    Text(card.comment)
                        .foregroundStyle(.secondary)
                        .textSelection(.enabled)
                }
            }
            .searchable(text: $searchText, prompt: "Search keywords, values, comments")
            .navigationTitle(document.fileName.isEmpty ? "FITS Header" : document.fileName)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarLeading) {
                    Button { close() } label: {
                        Image(systemName: "xmark")
                    }
                    .keyboardShortcut(.cancelAction)
                    .accessibilityLabel("Close")
                }
                ToolbarItem(placement: .topBarTrailing) {
                    Menu {
                        if let files = exportFiles {
                            ShareLink(item: files.txt) {
                                Label("Text File (.txt)", systemImage: "doc.plaintext")
                            }
                            ShareLink(item: files.csv) {
                                Label("Spreadsheet (.csv)", systemImage: "tablecells")
                            }
                        }
                    } label: {
                        Image(systemName: "square.and.arrow.up")
                    }
                    .disabled(exportFiles == nil)
                    .accessibilityLabel("Share header")
                }
            }
            .task { exportFiles = writeExportFiles() }
        }
    }

    private func close() {
        if let onClose {
            onClose()
        } else {
            dismissWindow()
        }
    }

    // MARK: Export

    private var baseName: String {
        let name = (document.fileName as NSString).deletingPathExtension
        return (name.isEmpty ? "FITS" : name) + "_header"
    }

    private func writeExportFiles() -> (txt: URL, csv: URL)? {
        let folder = FileManager.default.temporaryDirectory
        let txtURL = folder.appendingPathComponent(baseName + ".txt")
        let csvURL = folder.appendingPathComponent(baseName + ".csv")
        do {
            try textExport().write(to: txtURL, atomically: true, encoding: .utf8)
            try csvExport().write(to: csvURL, atomically: true, encoding: .utf8)
            return (txtURL, csvURL)
        } catch {
            return nil
        }
    }

    /// FITS-style cards: "KEYWORD = value / comment", one per line, ending with END.
    private func textExport() -> String {
        func pad(_ s: String, _ n: Int) -> String {
            s.count >= n ? s : s + String(repeating: " ", count: n - s.count)
        }
        var lines: [String] = []
        for card in document.cards {
            let keyword = pad(card.keyword, 8)
            guard card.isValue else {
                lines.append(card.comment.isEmpty ? card.keyword : keyword + card.comment)
                continue
            }
            var value: String
            if card.isString {
                value = pad("'" + card.value.replacingOccurrences(of: "'", with: "''") + "'", 20)
            } else {
                value = String(repeating: " ", count: max(0, 20 - card.value.count)) + card.value
            }
            if !card.comment.isEmpty { value += " / " + card.comment }
            lines.append(keyword + "= " + value)
        }
        lines.append("END")
        return lines.joined(separator: "\n") + "\n"
    }

    /// Index, Keyword, Value, Comment — every field quoted.
    private func csvExport() -> String {
        func quoted(_ s: String) -> String { "\"" + s.replacingOccurrences(of: "\"", with: "\"\"") + "\"" }
        var lines = ["Index,Keyword,Value,Comment"]
        for card in document.cards {
            lines.append([String(card.id + 1), quoted(card.keyword), quoted(card.value), quoted(card.comment)]
                .joined(separator: ","))
        }
        return lines.joined(separator: "\n") + "\n"
    }
}
