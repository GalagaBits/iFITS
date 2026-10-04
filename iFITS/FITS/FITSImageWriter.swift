//
//  FITSImageWriter.swift
//  iFITS Start
//
//  Writes FITS header cards and 32-bit float image HDUs.
//

import Foundation

/// Builds FITS image HDUs: 80-character header cards padded to 2880-byte blocks, then big-endian
/// 32-bit float data (BITPIX = -32), also padded to 2880 bytes.
nonisolated enum FITSImageWriter {
    static let block = 2880

    /// Keywords that describe the data layout. They're always written fresh, never copied.
    static let layoutKeywords: Set<String> = [
        "SIMPLE", "XTENSION", "BITPIX", "NAXIS", "EXTEND", "PCOUNT", "GCOUNT",
        "BSCALE", "BZERO", "BLANK", "END",
        // No longer true once the data change:
        "CHECKSUM", "DATASUM", "DATAMIN", "DATAMAX"
    ]

    // MARK: Cards

    /// Pads (or cuts) to exactly 80 ASCII characters.
    static func card(_ text: String) -> String {
        let ascii = String(text.unicodeScalars.map { $0.isASCII && $0.value >= 32 ? Character($0) : "?" })
        return ascii.count >= 80 ? String(ascii.prefix(80)) : ascii + String(repeating: " ", count: 80 - ascii.count)
    }

    static func keyword(_ k: String) -> String {
        let k = k.uppercased()
        return k.count >= 8 ? String(k.prefix(8)) : k + String(repeating: " ", count: 8 - k.count)
    }

    /// Fixed format: the value ends in column 30.
    private static func valueCard(_ k: String, _ value: String, _ comment: String) -> String {
        let field = value.count >= 20 ? value : String(repeating: " ", count: 20 - value.count) + value
        return card(keyword(k) + "= " + field + (comment.isEmpty ? "" : " / " + comment))
    }

    static func int(_ k: String, _ value: Int, _ comment: String = "") -> String {
        valueCard(k, String(value), comment)
    }

    static func bool(_ k: String, _ value: Bool, _ comment: String = "") -> String {
        valueCard(k, value ? "T" : "F", comment)
    }

    static func real(_ k: String, _ value: Double, _ comment: String = "") -> String {
        var text = String(format: "%.15G", value)
        if !text.contains(".") && !text.contains("E") && !text.contains("N") && !text.contains("I") {
            text += ".0"
        }
        return valueCard(k, text, comment)
    }

    /// A string value ('' for quotes inside), at least 8 characters inside the quotes.
    static func string(_ k: String, _ value: String, _ comment: String = "") -> String {
        // ASCII only, at most 68 characters inside the quotes, never splitting a '' pair.
        let ascii = String(value.unicodeScalars.map { $0.isASCII && $0.value >= 32 ? Character($0) : "?" })
        var inner = ""
        for ch in ascii {
            let add = ch == "'" ? "''" : String(ch)
            if inner.count + add.count > 68 { break }
            inner += add
        }
        if inner.count < 8 { inner += String(repeating: " ", count: 8 - inner.count) }
        let quoted = "'" + inner + "'"
        let field = quoted.count >= 20 ? quoted : quoted + String(repeating: " ", count: 20 - quoted.count)
        return card(keyword(k) + "= " + field + (comment.isEmpty ? "" : " / " + comment))
    }

    /// HISTORY or COMMENT text, split over as many cards as it needs.
    static func text(_ k: String, _ text: String) -> [String] {
        var cards: [String] = []
        var rest = Substring(text)
        repeat {
            let line = rest.prefix(72)
            cards.append(card(keyword(k) + String(line)))
            rest = rest.dropFirst(line.count)
        } while !rest.isEmpty
        return cards
    }

    // MARK: Headers

    /// An image header with fresh layout cards (BITPIX = -32), followed by every other card of
    /// `copying` (WCS, units, observation details, …) and then `extra`.
    /// - primary: true for the primary HDU (SIMPLE, EXTEND), false for an IMAGE extension.
    /// - extname: a new EXTNAME (replaces any copied one), or nil to keep the copied one.
    static func imageHeader(copying cards: [String], primary: Bool, axes: [Int], extname: String?,
                            alsoDrop: Set<String> = [], extra: [String] = []) -> [String] {
        var out: [String] = []
        if primary {
            out.append(bool("SIMPLE", true, "Standard FITS file"))
        } else {
            out.append(string("XTENSION", "IMAGE", "Image extension"))
        }
        out.append(int("BITPIX", -32, "32-bit floating point"))
        out.append(int("NAXIS", axes.count, "Number of axes"))
        for (i, n) in axes.enumerated() {
            out.append(int("NAXIS\(i + 1)", n, "Length of axis \(i + 1)"))
        }
        if primary {
            out.append(bool("EXTEND", true, "Extensions may follow"))
        } else {
            out.append(int("PCOUNT", 0, "No extra parameters"))
            out.append(int("GCOUNT", 1, "One group"))
        }
        var drop = layoutKeywords.union(alsoDrop)
        if primary { drop.insert("INHERIT") }          // only meaningful in extensions
        if let extname {
            out.append(string("EXTNAME", extname, "Extension name"))
            drop.insert("EXTNAME")
        }

        // Copy the rest. A CONTINUE card belongs to the card before it, so it goes (or stays) with it.
        var dropping = false
        for original in cards {
            let k = FITSHDUList.keyword(of: original)
            if k == "CONTINUE" {
                if !dropping { out.append(card(original)) }
                continue
            }
            let isAxisLength = k.hasPrefix("NAXIS") && Int(k.dropFirst(5)) != nil
            dropping = drop.contains(k) || isAxisLength
            if !dropping { out.append(card(original)) }
        }
        return out + extra
    }

    /// Header cards + END, padded with spaces to whole 2880-byte blocks.
    static func headerData(_ cards: [String]) -> Data {
        var text = (cards.map(card) + [card("END")]).joined()
        text += String(repeating: " ", count: (block - text.count % block) % block)
        return Data(text.utf8)
    }

    // MARK: Data

    /// Big-endian 32-bit floats written into `data` at `byteOffset`.
    static func write(_ values: [Float], into data: inout Data, at byteOffset: Int) {
        data.withUnsafeMutableBytes { (raw: UnsafeMutableRawBufferPointer) in
            guard let base = raw.baseAddress, byteOffset + values.count * 4 <= raw.count else { return }
            for (i, v) in values.enumerated() {
                base.storeBytes(of: v.bitPattern.bigEndian, toByteOffset: byteOffset + i * 4, as: UInt32.self)
            }
        }
    }

    /// Zero bytes that pad `byteCount` of data to a whole block.
    static func dataPadding(for byteCount: Int) -> Data {
        Data(count: (block - byteCount % block) % block)
    }
}
