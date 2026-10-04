//
//  UTType+FITS.swift
//  iFITS Start
//
//  The .fits file type and a string helper for 80-character cards.
//

import Foundation
import UniformTypeIdentifiers

extension UTType {
    static let fitsFile = UTType(filenameExtension: "fits")!
}

extension String {
    func chunked(into size: Int) -> [String] {
        var out: [String] = []
        var idx = startIndex
        while idx < endIndex {
            let endIdx = index(idx, offsetBy: size, limitedBy: endIndex) ?? endIndex
            out.append(String(self[idx..<endIdx]))
            idx = endIdx
        }
        return out
    }
}
