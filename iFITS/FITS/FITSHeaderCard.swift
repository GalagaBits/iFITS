//
//  FITSHeaderCard.swift
//  iFITS Start
//
//  One 80-character FITS header card, parsed.
//

import Foundation

/// One 80-character header card, in file order.
struct FITSHeaderCard: Codable, Hashable, Identifiable {
    var id: Int                 // position in the header (0-based)
    var keyword: String
    var value: String           // string values without their quotes
    var comment: String         // text after "/" (or the text of COMMENT / HISTORY cards)
    var isValue: Bool           // "KEYWORD = value" card
    var isString: Bool          // value was a quoted string

    /// Parses a card, keeping "/" inside quoted strings (e.g. BUNIT = 'MJy/sr').
    static func parse(_ card: String, index: Int) -> FITSHeaderCard {
        let chars = Array(card)
        let keyword = String(chars.prefix(8)).trimmingCharacters(in: .whitespaces)

        // COMMENT, HISTORY, blank and other cards without "= " in columns 9–10.
        guard chars.count > 10, chars[8] == "=", chars[9] == " " else {
            let text = chars.count > 8 ? String(chars[8...]).trimmingCharacters(in: .whitespaces) : ""
            return FITSHeaderCard(id: index, keyword: keyword, value: "", comment: text,
                                  isValue: false, isString: false)
        }

        let field = Array(chars[10...])
        var value = "", comment = ""
        var isString = false

        if let quote = field.firstIndex(of: "'"), field[..<quote].allSatisfy({ $0 == " " }) {
            // Quoted string: '' inside means a literal quote.
            isString = true
            var i = quote + 1
            var text = ""
            while i < field.count {
                if field[i] == "'" {
                    if i + 1 < field.count, field[i + 1] == "'" {
                        text.append("'")
                        i += 2
                        continue
                    }
                    i += 1
                    break
                }
                text.append(field[i])
                i += 1
            }
            value = text.trimmingCharacters(in: .whitespaces)
            let rest = String(field[min(i, field.count)...])
            if let slash = rest.firstIndex(of: "/") {
                comment = String(rest[rest.index(after: slash)...]).trimmingCharacters(in: .whitespaces)
            }
        } else {
            let text = String(field)
            if let slash = text.firstIndex(of: "/") {
                value = String(text[..<slash]).trimmingCharacters(in: .whitespaces)
                comment = String(text[text.index(after: slash)...]).trimmingCharacters(in: .whitespaces)
            } else {
                value = text.trimmingCharacters(in: .whitespaces)
            }
        }
        return FITSHeaderCard(id: index, keyword: keyword, value: value, comment: comment,
                              isValue: true, isString: isString)
    }
}
