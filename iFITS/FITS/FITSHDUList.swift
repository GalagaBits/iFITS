//
//  FITSHDUList.swift
//  iFITS Start
//
//  Lists every HDU in a FITS file: number, EXTNAME, type, BITPIX and shape (like fits.info()).
//

import Foundation

/// One HDU (header + data unit) of a FITS file: where it sits in the file and what it holds.
nonisolated struct FITSHDUInfo: Identifiable, Hashable, Sendable {
    /// 0 = primary HDU, 1 = first extension, …
    let index: Int
    /// Byte offset of the header, and of the data, in the file.
    let headerOffset: Int
    let dataOffset: Int
    /// Data bytes (without the padding to 2880).
    let dataBytes: Int
    /// The header's 80-character cards, in order, without END.
    let cards: [String]
    /// "KEYWORD = value" cards: keyword → value (strings without their quotes).
    let keys: [String: String]

    var id: Int { index }

    var xtension: String { (keys["XTENSION"] ?? "").trimmingCharacters(in: .whitespaces).uppercased() }
    var extname: String { (keys["EXTNAME"] ?? "").trimmingCharacters(in: .whitespaces) }
    var bitpix: Int { Int(keys["BITPIX"] ?? "") ?? 0 }
    var bscale: Double { FITSHDUList.number(keys["BSCALE"]) ?? 1 }
    var bzero: Double { FITSHDUList.number(keys["BZERO"]) ?? 0 }
    var unit: String { (keys["BUNIT"] ?? "").trimmingCharacters(in: .whitespaces) }

    /// NAXIS1, NAXIS2, … (empty when NAXIS = 0).
    var axes: [Int] {
        let naxis = Int(keys["NAXIS"] ?? "") ?? 0
        guard naxis > 0 else { return [] }
        return (1...naxis).map { Int(keys["NAXIS\($0)"] ?? "") ?? 0 }
    }

    /// An image iFITS can show: the primary HDU or an IMAGE extension, at least 2-D, with all of
    /// its data in the file. (Tables such as BINTABLE and compressed images are not images here.)
    var isImage: Bool {
        guard index == 0 || xtension == "IMAGE" else { return false }
        guard keys["ZIMAGE"] == nil else { return false }
        let a = axes
        guard a.count >= 2, a.allSatisfy({ $0 > 0 }) else { return false }
        guard [8, 16, 32, 64, -32, -64].contains(bitpix) else { return false }
        guard let bytes = FITSHDUList.product(a + [abs(bitpix) / 8]) else { return false }
        return dataBytes == bytes && dataBytes > 0
    }

    /// "SCI", or "PRIMARY" / "HDU 3" when there's no EXTNAME.
    var name: String {
        if !extname.isEmpty { return extname }
        return index == 0 ? "PRIMARY" : "HDU \(index)"
    }

    /// "1: SCI", like CARTA's HDU menu.
    var label: String { "\(index): \(name)" }

    /// "45 × 47 × 1213"
    var shapeText: String { axes.map(String.init).joined(separator: " × ") }

    /// Planes after the first two axes (1 for a 2-D image).
    var planeCount: Int { axes.dropFirst(2).reduce(1, *) }

    /// "float32", "int16 (scaled)", …
    var dataTypeText: String {
        let base: String
        switch bitpix {
        case 8: base = "uint8"
        case 16: base = bzero == 32768 && bscale == 1 ? "uint16" : "int16"
        case 32: base = bzero == 2_147_483_648 && bscale == 1 ? "uint32" : "int32"
        case 64: base = "int64"
        case -32: base = "float32"
        case -64: base = "float64"
        default: base = "BITPIX \(bitpix)"
        }
        let unsignedTrick = (bitpix == 16 && bzero == 32768) || (bitpix == 32 && bzero == 2_147_483_648)
        if (bscale != 1 || bzero != 0) && !unsignedTrick { return base + " (scaled)" }
        return base
    }

    /// One line for lists: "45 × 47 × 1213 · float32 · MJy/sr".
    var summary: String {
        [shapeText, dataTypeText, unit].filter { !$0.isEmpty }.joined(separator: " · ")
    }
}

nonisolated enum FITSHDUList {
    static let block = 2880

    /// Every HDU in the file, in order. Stops quietly at the first damaged or missing header.
    static func scan(_ data: Data) -> [FITSHDUInfo] {
        var hdus: [FITSHDUInfo] = []
        data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) in
            let count = raw.count
            var offset = 0
            var index = 0
            while offset + block <= count {
                let start = offset
                var cards: [String] = []
                var keys: [String: String] = [:]
                var foundEnd = false

                while !foundEnd, offset + block <= count {
                    for i in 0..<36 {
                        let s = offset + i * 80
                        let card = String(decoding: UnsafeRawBufferPointer(rebasing: raw[s..<(s + 80)]), as: UTF8.self)
                        let key = keyword(of: card)
                        if key == "END" {
                            foundEnd = true
                            break
                        }
                        cards.append(card)
                        if let kv = keyValue(card) { keys[kv.key] = kv.value }
                    }
                    offset += block
                }
                // The first card must be SIMPLE (primary) or XTENSION (extensions).
                guard foundEnd, let first = cards.first.map(keyword(of:)),
                      first == (index == 0 ? "SIMPLE" : "XTENSION") else { break }

                // Data size = |BITPIX|/8 × GCOUNT × (PCOUNT + NAXIS1 × NAXIS2 × …). A damaged header
                // with absurd sizes ends the scan instead of overflowing.
                let naxis = min(999, max(0, Int(keys["NAXIS"] ?? "") ?? 0))
                let bytesPerValue = abs(Int(keys["BITPIX"] ?? "") ?? 0) / 8
                let lengths = naxis > 0 ? (1...naxis).map { max(0, Int(keys["NAXIS\($0)"] ?? "") ?? 0) } : []
                let pcount = max(0, Int(keys["PCOUNT"] ?? "") ?? 0)
                let gcount = max(0, Int(keys["GCOUNT"] ?? "") ?? 1)
                var dataBytes = 0
                if naxis > 0 {
                    guard let elements = product(lengths) else { break }
                    let withHeap = elements.addingReportingOverflow(pcount)
                    guard !withHeap.overflow,
                          let bytes = product([bytesPerValue, gcount, withHeap.partialValue]) else { break }
                    dataBytes = bytes
                }
                let dataStart = offset

                hdus.append(FITSHDUInfo(index: index, headerOffset: start, dataOffset: dataStart,
                                        dataBytes: min(dataBytes, max(0, count - dataStart)), cards: cards, keys: keys))
                // (A file cut short just ends the loop: the next header would start past the end.)
                guard dataBytes <= Int.max - dataStart - block else { break }
                let end = dataStart + (dataBytes + block - 1) / block * block
                guard end > start else { break }
                offset = end
                index += 1
            }
        }
        return hdus
    }

    /// Columns 1–8 of a card, trimmed and upper-cased.
    static func keyword(of card: String) -> String {
        String(card.prefix(8)).trimmingCharacters(in: .whitespaces).uppercased()
    }

    /// "KEYWORD = value / comment" → (KEYWORD, value). Strings lose their quotes ('' → ').
    static func keyValue(_ card: String) -> (key: String, value: String)? {
        let chars = Array(card)
        guard chars.count >= 10, chars[8] == "=", chars[9] == " " else { return nil }
        let key = String(chars[0..<8]).trimmingCharacters(in: .whitespaces).uppercased()
        guard !key.isEmpty else { return nil }
        let field = Array(chars[10...])
        if let quote = field.firstIndex(of: "'"), field[..<quote].allSatisfy({ $0 == " " }) {
            var text = ""
            var i = quote + 1
            while i < field.count {
                if field[i] == "'" {
                    if i + 1 < field.count, field[i + 1] == "'" {
                        text.append("'")
                        i += 2
                        continue
                    }
                    break
                }
                text.append(field[i])
                i += 1
            }
            return (key, text.trimmingCharacters(in: .whitespaces))
        }
        var text = String(field)
        if let slash = text.firstIndex(of: "/") { text = String(text[..<slash]) }
        return (key, text.trimmingCharacters(in: .whitespaces))
    }

    /// The product of the numbers, or nil if it overflows.
    static func product(_ values: [Int]) -> Int? {
        var result = 1
        for v in values {
            let (p, overflow) = result.multipliedReportingOverflow(by: v)
            if overflow { return nil }
            result = p
        }
        return result
    }

    /// A header number ("1.5", "1.5D3", "  2 ") as a Double.
    static func number(_ text: String?) -> Double? {
        guard let text else { return nil }
        return Double(text.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: "D", with: "E"))
    }
}
