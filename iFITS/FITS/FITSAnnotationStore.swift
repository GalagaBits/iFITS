//
//  FITSAnnotationStore.swift
//  iFITS Start
//
//  Reads and writes the APPLE_PENCIL_ANNOTATIONS and DS9_REGIONS extensions.
//

import SwiftUI
import PencilKit

/// Reads and writes iFITS's two extensions. They're appended at the end of the file, so the
/// existing HDUs stay byte-for-byte unchanged (other FITS software just sees extra extensions):
/// • APPLE_PENCIL_ANNOTATIONS: a 1-D, 8-bit IMAGE extension holding the PencilKit drawing
///   (PKDrawing.dataRepresentation).
/// • DS9_REGIONS: a binary table with one text column, REGION, holding one DS9 region line per
///   row in image pixel coordinates. Joined with newlines, the rows are a valid DS9 .reg file
///   (e.g. in Python: "\n".join(Table.read(f, hdu="DS9_REGIONS")["REGION"])).
nonisolated enum FITSAnnotationStore {
    static let extensionName = "APPLE_PENCIL_ANNOTATIONS"
    static let regionExtensionName = "DS9_REGIONS"
    private static let block = 2880

    private struct HDU {
        let range: Range<Int>       // whole HDU (header + padded data), byte offsets
        let dataRange: Range<Int>   // data bytes only
        let keys: [String: String]
        var extname: String { (keys["EXTNAME"] ?? "").uppercased() }
    }

    /// The saved PencilKit data, if the file has an APPLE_PENCIL_ANNOTATIONS extension.
    static func readDrawingData(from fits: Data) -> Data? {
        let data = fits.startIndex == 0 ? fits : Data(fits)     // no copy for whole files
        guard let hdu = scan(data).last(where: { $0.extname == extensionName }),
              !hdu.dataRange.isEmpty else { return nil }
        return data.subdata(in: hdu.dataRange)
    }

    /// The saved regions as DS9 region-file text, if the file has a DS9_REGIONS extension.
    static func readRegionText(from fits: Data) -> String? {
        let data = fits.startIndex == 0 ? fits : Data(fits)     // no copy for whole files
        guard let hdu = scan(data).last(where: { $0.extname == regionExtensionName }) else { return nil }
        let rowBytes = Int(hdu.keys["NAXIS1"] ?? "") ?? 0
        let rows = Int(hdu.keys["NAXIS2"] ?? "") ?? 0
        guard rowBytes > 0, rows > 0, !hdu.dataRange.isEmpty else { return nil }
        let table = data.subdata(in: hdu.dataRange)
        var lines: [String] = []
        for row in 0..<rows {
            let start = row * rowBytes
            guard start + rowBytes <= table.count else { break }
            let text = String(decoding: table[start..<(start + rowBytes)], as: UTF8.self)
            lines.append(text.trimmingCharacters(in: CharacterSet(charactersIn: " \0")))
        }
        return lines.joined(separator: "\n")
    }

    /// The file with any old iFITS extensions removed and the current ones appended
    /// (an extension is left out when there's nothing to put in it).
    static func updating(_ fits: Data, drawing: Data?, regionLines: [String]) -> Data {
        var out = Data(fits)
        for hdu in scan(out).reversed()
        where hdu.extname == extensionName || hdu.extname == regionExtensionName {
            out.removeSubrange(hdu.range)
        }
        if let drawing, !drawing.isEmpty {
            out.append(makeDrawingHDU(drawing))
        }
        if !regionLines.isEmpty {
            out.append(makeRegionHDU(regionLines))
        }
        return out
    }

    /// Just the iFITS extensions (none if there's nothing to save), to append to a new file.
    static func extensionHDUs(drawing: Data?, regionLines: [String]) -> Data {
        var out = Data()
        if let drawing, !drawing.isEmpty { out.append(makeDrawingHDU(drawing)) }
        if !regionLines.isEmpty { out.append(makeRegionHDU(regionLines)) }
        return out
    }

    // MARK: Reading HDU layout

    private static func scan(_ data: Data) -> [HDU] {
        var hdus: [HDU] = []
        let count = data.count
        var offset = 0

        while offset + block <= count {
            let start = offset
            var keys: [String: String] = [:]
            var foundEnd = false

            while !foundEnd, offset + block <= count {
                for i in 0..<36 {
                    let s = offset + i * 80
                    let card = String(decoding: data[s..<(s + 80)], as: UTF8.self)
                    if card.prefix(8).trimmingCharacters(in: .whitespaces) == "END" {
                        foundEnd = true
                        break
                    }
                    if let kv = keyValue(card) { keys[kv.0] = kv.1 }
                }
                offset += block
            }
            guard foundEnd else { break }

            let naxis = Int(keys["NAXIS"] ?? "") ?? 0
            let bytesPerValue = abs(Int(keys["BITPIX"] ?? "") ?? 0) / 8
            var elements = 0
            if naxis > 0 {
                elements = 1
                for i in 1...naxis { elements *= Int(keys["NAXIS\(i)"] ?? "") ?? 0 }
            }
            let pcount = Int(keys["PCOUNT"] ?? "") ?? 0
            let gcount = Int(keys["GCOUNT"] ?? "") ?? 1
            let dataBytes = naxis > 0 ? bytesPerValue * gcount * (pcount + elements) : 0

            let dataStart = offset
            let end = min(dataStart + (dataBytes + block - 1) / block * block, count)
            hdus.append(HDU(range: start..<end,
                            dataRange: dataStart..<min(dataStart + dataBytes, count),
                            keys: keys))
            guard end > start else { break }
            offset = end
        }
        return hdus
    }

    /// "KEYWORD = value / comment" → (KEYWORD, value), with quotes removed from strings.
    private static func keyValue(_ card: String) -> (String, String)? {
        let chars = Array(card)
        guard chars.count >= 10, chars[8] == "=", chars[9] == " " else { return nil }
        let key = String(chars[0..<8]).trimmingCharacters(in: .whitespaces)
        var rest = String(chars[10...]).trimmingCharacters(in: .whitespaces)
        if rest.hasPrefix("'") {
            rest.removeFirst()
            if let quote = rest.firstIndex(of: "'") { rest = String(rest[..<quote]) }
        } else if let slash = rest.firstIndex(of: "/") {
            rest = String(rest[..<slash])
        }
        return (key, rest.trimmingCharacters(in: .whitespaces))
    }

    // MARK: Writing the extensions

    private static func card(_ text: String) -> String {
        text.count >= 80 ? String(text.prefix(80)) : text + String(repeating: " ", count: 80 - text.count)
    }

    private static func keyword(_ k: String) -> String {
        k.count >= 8 ? String(k.prefix(8)) : k + String(repeating: " ", count: 8 - k.count)
    }

    private static func number(_ k: String, _ value: Int, _ comment: String) -> String {
        let v = String(value)
        return card(keyword(k) + "= " + String(repeating: " ", count: max(0, 20 - v.count)) + v + " / " + comment)
    }

    private static func string(_ k: String, _ value: String, _ comment: String) -> String {
        let inner = value.count >= 8 ? value : value + String(repeating: " ", count: 8 - value.count)
        let quoted = "'" + inner + "'"
        let field = quoted.count >= 20 ? quoted : quoted + String(repeating: " ", count: 20 - quoted.count)
        return card(keyword(k) + "= " + field + " / " + comment)
    }

    /// Header cards + END, padded to whole 2880-byte blocks.
    private static func headerData(_ cards: [String]) -> Data {
        var header = (cards + [card("END")]).joined()
        header += String(repeating: " ", count: (block - header.count % block) % block)
        return Data(header.utf8)
    }

    private static func makeDrawingHDU(_ payload: Data) -> Data {
        var hdu = headerData([
            string("XTENSION", "IMAGE", "Image extension"),
            number("BITPIX", 8, "8-bit bytes"),
            number("NAXIS", 1, "Number of axes"),
            number("NAXIS1", payload.count, "Bytes of PencilKit drawing data"),
            number("PCOUNT", 0, "No extra parameters"),
            number("GCOUNT", 1, "One group"),
            string("EXTNAME", extensionName, "Apple Pencil annotations"),
            card("COMMENT Apple PencilKit drawing (PKDrawing.dataRepresentation) saved by iFITS."),
            card("COMMENT Units are image pixels: x to the right, y down from the top-left of the"),
            card("COMMENT image as displayed (FITS row NAXIS2 at the top, row 1 at the bottom).")
        ])
        hdu.append(payload)
        hdu.append(Data(count: (block - payload.count % block) % block))
        return hdu
    }

    private static func makeRegionHDU(_ lines: [String]) -> Data {
        // FITS text columns are ASCII; anything else becomes "?".
        let rows: [[UInt8]] = lines.map { line in
            line.unicodeScalars.map { $0.isASCII && $0.value >= 32 ? UInt8($0.value) : UInt8(ascii: "?") }
        }
        let width = max(1, rows.map(\.count).max() ?? 1)
        var hdu = headerData([
            string("XTENSION", "BINTABLE", "Binary table extension"),
            number("BITPIX", 8, "8-bit bytes"),
            number("NAXIS", 2, "2-dimensional table"),
            number("NAXIS1", width, "Bytes per row"),
            number("NAXIS2", rows.count, "Number of rows"),
            number("PCOUNT", 0, "No heap"),
            number("GCOUNT", 1, "One group"),
            number("TFIELDS", 1, "One column"),
            string("TTYPE1", "REGION", "DS9 region file line"),
            string("TFORM1", "\(width)A", "Text"),
            string("EXTNAME", regionExtensionName, "DS9 regions"),
            card("COMMENT DS9 region file saved by iFITS: one line per row, image pixel"),
            card("COMMENT coordinates. Join the rows with newlines to get a .reg file.")
        ])
        var table = Data(capacity: width * rows.count)
        for row in rows {
            table.append(contentsOf: row)
            table.append(contentsOf: [UInt8](repeating: UInt8(ascii: " "), count: width - row.count))
        }
        hdu.append(table)
        hdu.append(Data(count: (block - table.count % block) % block))
        return hdu
    }
}
