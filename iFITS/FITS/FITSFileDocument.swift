//
//  FITSFileDocument.swift
//  iFITS Start
//
//  A FITS file's bytes, for "Save as Copy".
//

import SwiftUI
import UniformTypeIdentifiers

/// A FITS file's bytes, for "Save as Copy…".
nonisolated struct FITSFileDocument: FileDocument {
    /// The type of files ending in .fits (so copies are saved with that extension).
    static let fitsType = UTType(filenameExtension: "fits") ?? .data
    static var readableContentTypes: [UTType] { [fitsType] }

    var data: Data

    init(data: Data) {
        self.data = data
    }

    init(configuration: ReadConfiguration) throws {
        data = configuration.file.regularFileContents ?? Data()
    }

    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        FileWrapper(regularFileWithContents: data)
    }
}
