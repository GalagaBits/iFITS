//
//  FITSDecoder.swift
//  iFITS Start
//
//  Reads a FITS file: header blocks, data offset, BITPIX decoding of the first image HDU.
//

import Foundation

class FITSDecoder {

    // Result structure to hold parsed FITS data
    struct Result {
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
    }

    // Extracted from ContentView
    static func loadFITS(with data: Data) throws -> Result {
        var offset = 0
        var finalResult: Result?

        while offset < data.count {
            let hduStartOffset = offset

            var headerData = Data()
            var headerString = ""
            var endCardFound = false

            while offset < data.count {
                let blockEnd = min(offset + 2880, data.count)
                guard blockEnd > offset else { break }

                let currentBlockData = data[offset..<blockEnd]
                headerData.append(currentBlockData)
                offset = blockEnd

                if let cumulativeString = String(data: headerData, encoding: .ascii) {
                    if cumulativeString.contains("END" + String(repeating: " ", count: 77)) {
                        headerString = cumulativeString
                        endCardFound = true
                        break
                    }
                } else {
                    throw NSError(domain: "FITS", code: -6, userInfo: [NSLocalizedDescriptionKey: "Corrupt data found while reading header."])
                }
            }

            guard endCardFound else { break }

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
                dataSize = (abs(bitpix) / 8) * elements
            }

            let w = Int(currentHeaderDict["NAXIS1"] ?? "0") ?? 0
            let h = Int(currentHeaderDict["NAXIS2"] ?? "0") ?? 0

            if naxis >= 2 && w > 0 && h > 0 && bitpix != 0 {

                let loadedBscale = Double(currentHeaderDict["BSCALE"] ?? "1.0") ?? 1.0
                let loadedBzero = Double(currentHeaderDict["BZERO"] ?? "0.0") ?? 0.0

                guard dataStartOffset + dataSize <= data.count else {
                    throw NSError(domain: "FITS", code: -2, userInfo: [NSLocalizedDescriptionKey: "Image data incomplete."])
                }

                let pixelsInSlice = w * h
                let bytesPerPixel = abs(bitpix) / 8
                let sliceDataSize = pixelsInSlice * bytesPerPixel

                let slice = data[dataStartOffset..<(dataStartOffset + sliceDataSize)]

                var floats = [Float](repeating: 0, count: pixelsInSlice)

                slice.withUnsafeBytes { buf in
                    guard let base = buf.baseAddress else { return }
                    DispatchQueue.concurrentPerform(iterations: pixelsInSlice) { i in
                        let ptr = base.advanced(by: i * bytesPerPixel)
                        let rv: Float
                        switch bitpix {
                        case 8: rv = Float(ptr.load(as: UInt8.self))
                        case 16: rv = Float(Int16(bitPattern: UInt16(bigEndian: ptr.load(as: UInt16.self))))
                        case 32: rv = Float(Int32(bitPattern: UInt32(bigEndian: ptr.load(as: UInt32.self))))
                        case -32: rv = Float(bitPattern: UInt32(bigEndian: ptr.load(as: UInt32.self)))
                        case -64: rv = Float(Double(bitPattern: UInt64(bigEndian: ptr.load(as: UInt64.self))))
                        case 64: rv = Float(Int64(bitPattern: UInt64(bigEndian: ptr.load(as: UInt64.self))))
                        default: rv = 0
                        }
                        floats[i] = rv
                    }
                }

                let axisLengths = (1...naxis).map { Int(currentHeaderDict["NAXIS\($0)"] ?? "") ?? 0 }
                finalResult = Result(floats: floats, width: w, height: h, bscale: loadedBscale, bzero: loadedBzero,
                                     headerDict: currentHeaderDict, headerCards: currentCards,
                                     dataOffset: dataStartOffset, bitpix: bitpix, axisLengths: axisLengths)
                break // Exit after loading the first valid HDU
            }

            let paddedDataSize = (dataSize + 2879) / 2880 * 2880
            offset = dataStartOffset + paddedDataSize

            if offset <= hduStartOffset {
                throw NSError(domain: "FITS", code: -3, userInfo: [NSLocalizedDescriptionKey: "Failed to advance in FITS file, file may be corrupt."])
            }
        }

        if let res = finalResult {
            return res
        } else {
            throw NSError(domain: "FITS", code: -1, userInfo: [NSLocalizedDescriptionKey: "No 2D image HDU found in the file."])
        }
    }
}
