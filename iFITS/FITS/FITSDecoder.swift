//
//  FITSDecoder.swift
//  iFITS Start
//
//  Reads a FITS file: header blocks, data offset, and BITPIX decoding of the chosen image HDU.
//

import Foundation

class FITSDecoder {

    // Result structure to hold parsed FITS data
    struct Result {
        /// The first plane's pixels in physical units (BSCALE, BZERO, BLANK applied), rows flipped
        /// for display (FITS row 1 at the bottom), ready to show.
        var floats: [Float]
        var width: Int
        var height: Int
        var bscale: Double
        var bzero: Double
        var headerDict: [String: String]
        /// Every header card of the loaded HDU, in order (for the header window).
        var headerCards: [FITSHeaderCard] = []
        /// Byte offset of the HDU's data in the file, its BITPIX, and NAXIS1, NAXIS2, NAXIS3, …
        /// Used to read the other planes of a cube (NAXIS ≥ 3) when they're shown.
        var dataOffset: Int = 0
        var bitpix: Int = 0
        var axisLengths: [Int] = []
        /// Which HDU this is (0 = primary).
        var hduIndex: Int = 0
    }

    /// Loads HDU number `hdu` (0 = primary), or the first image HDU when `hdu` is nil.
    /// Only the first plane is decoded; cubes read their other planes later.
    static func loadFITS(with data: Data, hdu target: Int? = nil) throws -> Result {
        var offset = 0
        var finalResult: Result?
        var hduIndex = 0

        while offset < data.count {
            let hduStartOffset = offset

            var headerData = Data()
            var headerString = ""
            var endCardFound = false

            while offset < data.count {
                let blockEnd = min(offset + 2880, data.count)
                guard blockEnd > offset else { break }

                let blockStart = offset
                headerData.append(data[blockStart..<blockEnd])
                offset = blockEnd

                // Only this block's 36 cards are checked for END (the whole header so far used to be
                // turned into text again after every block, which is slow for long JWST headers).
                if FITSDecoder.blockHasEndCard(data, from: blockStart, to: blockEnd) {
                    endCardFound = true
                    break
                }
            }

            guard endCardFound else { break }
            guard let text = String(data: headerData, encoding: .ascii) else {
                throw NSError(domain: "FITS", code: -6, userInfo: [NSLocalizedDescriptionKey: "Corrupt data found while reading header."])
            }
            headerString = text

            let dataStartOffset = offset

            let cards = headerString.chunked(into: 80)
            var currentHeaderDict = [String: String]()
            var currentCards: [FITSHeaderCard] = []
            for (index, card) in cards.enumerated() {
                if String(card.prefix(8)).trimmingCharacters(in: .whitespaces) == "END" { break }
                let parsed = FITSHeaderCard.parse(card, index: index)
                currentCards.append(parsed)
                if parsed.isValue, !parsed.keyword.isEmpty {
                    currentHeaderDict[parsed.keyword] = parsed.value
                }
            }

            let naxis = Int(currentHeaderDict["NAXIS"] ?? "0") ?? 0
            let bitpix = Int(currentHeaderDict["BITPIX"] ?? "0") ?? 0
            var dataSize = 0

            if naxis > 0 && bitpix != 0 {
                var elements = 1
                for i in 1...naxis {
                    guard let naxis_i_str = currentHeaderDict["NAXIS\(i)"], let naxis_i = Int(naxis_i_str) else {
                        throw NSError(domain: "FITS", code: -5, userInfo: [NSLocalizedDescriptionKey: "Corrupt HDU: NAXIS=\(naxis) but NAXIS\(i) is missing or invalid."])
                    }
                    elements *= naxis_i
                }
                // Tables can have a heap (PCOUNT bytes) after the rows.
                let pcount = Int(currentHeaderDict["PCOUNT"] ?? "0") ?? 0
                let gcount = Int(currentHeaderDict["GCOUNT"] ?? "1") ?? 1
                dataSize = (abs(bitpix) / 8) * gcount * (pcount + elements)
            }

            let w = Int(currentHeaderDict["NAXIS1"] ?? "0") ?? 0
            let h = Int(currentHeaderDict["NAXIS2"] ?? "0") ?? 0
            let xtension = (currentHeaderDict["XTENSION"] ?? "").trimmingCharacters(in: .whitespaces).uppercased()
            let isImage = naxis >= 2 && w > 0 && h > 0 && bitpix != 0 && (hduIndex == 0 || xtension == "IMAGE")

            if let target, hduIndex == target, !isImage {
                throw NSError(domain: "FITS", code: -7, userInfo: [NSLocalizedDescriptionKey: "HDU \(target) isn't an image."])
            }

            if isImage && (target == nil || target == hduIndex) {

                let loadedBscale = Double(currentHeaderDict["BSCALE"] ?? "1.0") ?? 1.0
                let loadedBzero = Double(currentHeaderDict["BZERO"] ?? "0.0") ?? 0.0

                guard dataStartOffset + dataSize <= data.count else {
                    throw NSError(domain: "FITS", code: -2, userInfo: [NSLocalizedDescriptionKey: "Image data incomplete."])
                }

                let pixelsInSlice = w * h
                let bytesPerPixel = abs(bitpix) / 8
                let sliceDataSize = pixelsInSlice * bytesPerPixel

                let slice = data[dataStartOffset..<(dataStartOffset + sliceDataSize)]

                // Decoded, converted to physical values and flipped for display in one parallel pass
                // (this used to start one task per pixel, then copy the image twice more).
                let floats: [Float] = slice.withUnsafeBytes { buf in
                    guard let base = buf.baseAddress else { return [Float](repeating: .nan, count: pixelsInSlice) }
                    return FITSPhysical.displayPixels(from: base, bitpix: bitpix, width: w, height: h,
                                                      bscale: loadedBscale, bzero: loadedBzero,
                                                      header: currentHeaderDict)
                }

                let axisLengths = (1...naxis).map { Int(currentHeaderDict["NAXIS\($0)"] ?? "") ?? 0 }
                finalResult = Result(floats: floats, width: w, height: h, bscale: loadedBscale, bzero: loadedBzero,
                                     headerDict: currentHeaderDict, headerCards: currentCards,
                                     dataOffset: dataStartOffset, bitpix: bitpix, axisLengths: axisLengths,
                                     hduIndex: hduIndex)
                break // Exit after loading the chosen HDU
            }

            let paddedDataSize = (dataSize + 2879) / 2880 * 2880
            offset = dataStartOffset + paddedDataSize
            hduIndex += 1

            if offset <= hduStartOffset {
                throw NSError(domain: "FITS", code: -3, userInfo: [NSLocalizedDescriptionKey: "Failed to advance in FITS file, file may be corrupt."])
            }
        }

        if let res = finalResult {
            return res
        } else if let target {
            throw NSError(domain: "FITS", code: -8, userInfo: [NSLocalizedDescriptionKey: "The file has no HDU \(target)."])
        } else {
            throw NSError(domain: "FITS", code: -1, userInfo: [NSLocalizedDescriptionKey: "No 2D image HDU found in the file."])
        }
    }

    /// Whether the 2880-byte header block [start, end) has an END card: a card (every 80 bytes)
    /// that is "END" followed by spaces.
    static func blockHasEndCard(_ data: Data, from start: Int, to end: Int) -> Bool {
        var card = start
        while card + 80 <= end {
            if data[card] == UInt8(ascii: "E"), data[card + 1] == UInt8(ascii: "N"), data[card + 2] == UInt8(ascii: "D") {
                var blank = true
                for i in (card + 3)..<(card + 80) where data[i] != UInt8(ascii: " ") {
                    blank = false
                    break
                }
                if blank { return true }
            }
            card += 80
        }
        return false
    }
}
