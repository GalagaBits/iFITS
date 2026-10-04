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
