//
//  UTType+FITS.swift
//  iFITS Start
//
//  The FITS file type and a string helper for 80-character cards.
//

import Foundation
import UniformTypeIdentifiers

extension UTType {
    /// iFITS's name for the FITS file type. It must match the identifier under Imported Type
    /// Identifiers and Document Types in the target's Info tab: that's what tells iPadOS which files
    /// are FITS files (.fits, .fit, .fts) and that iFITS opens them (Files' Open With, the share sheet).
    /// `nonisolated` because the project makes everything main-actor by default, and this constant is
    /// also read off the main thread (drag and drop, FITSFileDocument).
    nonisolated static let fitsFile = UTType(importedAs: "gov.nasa.gsfc.fits", conformingTo: .data)

    /// Every type a FITS file might have, for the file picker: iFITS's own, any other app's
    /// declaration for the same extensions, and the plain "files ending in .fits" type. So FITS files
    /// are never greyed out, even if the Info tab's declaration is missing.
    nonisolated static let fitsFileTypes: [UTType] = {
        var types: [UTType] = [.fitsFile]
        for ext in FITSFileName.extensions {
            var matches = UTType.types(tag: ext, tagClass: .filenameExtension, conformingTo: nil)
            if let byExtension = UTType(filenameExtension: ext) { matches.append(byExtension) }
            for type in matches where !types.contains(type) {
                types.append(type)
            }
        }
        return types
    }()
}

/// FITS file names.
nonisolated enum FITSFileName {
    static let extensions = ["fits", "fit", "fts"]

    /// "ngc2403.fits", "image.FIT", …
    static func matches(_ name: String) -> Bool {
        extensions.contains((name as NSString).pathExtension.lowercased())
    }
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
