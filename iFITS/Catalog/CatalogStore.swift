//
//  CatalogStore.swift
//  iFITS
//
//  The FITS catalog's data:
//  1. downloads catalog.sqlite from the catalog repo on GitHub (or uses the copy from last time),
//  2. reads its fitsfiles table with SQLite, which is built into iOS,
//  3. remembers which entries you looked at (Recents),
//  4. downloads the FITS files the catalog lists.
//

import Foundation
import SQLite3        // Apple's built-in copy of SQLite
import Observation    // @Observable: views update when the store's properties change

/// One row of the fitsfiles table.
nonisolated struct CatalogEntry: Identifiable, Hashable, Sendable {
    let id: Int64                                         // the row's number in the table
    let target, telescope, kind, archive, link: String    // link = the download_link column

    var isCube: Bool { kind == "cube" }
    var kindName: String { isCube ? "3D cube" : "Image" }

    /// The download link as a URL. iOS only downloads https links, so http becomes https.
    var url: URL? { URL(string: link.replacingOccurrences(of: "http://", with: "https://")) }

    /// The file's name: everything after the last "/" in the link, e.g. "HorseHead.fits".
    /// For MAST links that's the name at the end of "?uri=mast:JWST/product/….fits".
    var fileName: String { (link as NSString).lastPathComponent }
}

@Observable
final class CatalogStore {
    /// Where the app downloads the catalog: catalog.sqlite in the main-catalog release.
    static let catalogURL = URL(string: "https://github.com/GalagaBits/iFITS-Catalog-Database/releases/download/main-catalog/catalog.sqlite")!

    /// Every catalog entry, newest first.
    private(set) var entries: [CatalogEntry] = []
    /// True while the catalog is downloading.
    private(set) var isLoading = false
    /// Entries whose FITS file is downloading right now (by id).
    private(set) var downloading: Set<Int64> = []
    /// Download links of the entries you looked at, newest first. Saved between launches.
    private(set) var recents = UserDefaults.standard.stringArray(forKey: "catalogRecents") ?? []
    /// A problem to show in an alert (nil when there's none).
    var errorMessage: String?
    /// Goes up after each download or move, so the views check `localFile(for:)` again.
    private var filesChanged = 0

    /// The saved copy of the catalog (Application Support is hidden from the Files app).
    private let savedCatalog = URL.applicationSupportDirectory.appending(path: "catalog.sqlite")
    /// Downloaded FITS files go here: Files › On My iPad › iFITS › Catalog.
    private let downloadsFolder = URL.documentsDirectory.appending(path: "Catalog")

    /// The entries in Recents, in the order you looked at them.
    var recentEntries: [CatalogEntry] {
        recents.compactMap { link in entries.first { $0.link == link } }
    }

    // MARK: The catalog

    /// Downloads the newest catalog (it's only a few kilobytes) and reads it. When offline, it
    /// reads the copy saved last time instead.
    func load() async {
        guard !isLoading else { return }     // already loading
        isLoading = true
        defer { isLoading = false }          // runs when this function ends, however it ends

        do {
            // Always ask GitHub (not a cached copy), so new entries show up right away.
            let request = URLRequest(url: Self.catalogURL, cachePolicy: .reloadIgnoringLocalCacheData)
            let (data, response) = try await URLSession.shared.data(for: request)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            try FileManager.default.createDirectory(at: savedCatalog.deletingLastPathComponent(),
                                                    withIntermediateDirectories: true)
            try data.write(to: savedCatalog, options: .atomic)
        } catch {
            // Offline or GitHub unreachable: carry on with the copy from last time, if any.
            if !exists(savedCatalog) {
                errorMessage = "Couldn't download the catalog. Check your connection and try again."
                return
            }
        }

        do {
            entries = try readCatalog(at: savedCatalog)
        } catch {
            errorMessage = "Couldn't read the catalog."
        }
    }

    /// Remembers an entry you looked at: moves it to the front of Recents (10 at most).
    func recordViewed(_ entry: CatalogEntry) {
        recents.removeAll { $0 == entry.link }
        recents = Array(([entry.link] + recents).prefix(10))
        UserDefaults.standard.set(recents, forKey: "catalogRecents")
    }

    // MARK: FITS files

    /// The downloaded copy of an entry's FITS file, or nil if it isn't downloaded.
    func localFile(for entry: CatalogEntry) -> URL? {
        _ = filesChanged                     // reading this makes views re-check after a download
        let file = downloadsFolder.appending(path: entry.fileName)
        return exists(file) ? file : nil
    }

    /// Downloads an entry's FITS file into Documents › Catalog and returns where it is
    /// (or nil if it failed). A file that's already downloaded isn't downloaded again.
    @discardableResult
    func download(_ entry: CatalogEntry) async -> URL? {
        if let file = localFile(for: entry) { return file }
        guard let url = entry.url else { return nil }

        downloading.insert(entry.id)                 // shows a spinner on this entry
        defer { downloading.remove(entry.id) }
        do {
            // URLSession saves the download to a temporary file; we move it to our folder.
            let (temporary, response) = try await URLSession.shared.download(from: url)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            try FileManager.default.createDirectory(at: downloadsFolder, withIntermediateDirectories: true)
            let file = downloadsFolder.appending(path: entry.fileName)
            try FileManager.default.moveItem(at: temporary, to: file)
            filesChanged += 1
            return file
        } catch {
            errorMessage = "Couldn't download \(entry.target): \(error.localizedDescription)"
            return nil
        }
    }

    /// Call after a downloaded file was moved to another folder ("Save to a Folder…").
    func fileMoved() { filesChanged += 1 }

    /// Whether a file exists. (`path(percentEncoded: false)` keeps spaces as spaces, as in
    /// "Application Support".)
    private func exists(_ url: URL) -> Bool {
        FileManager.default.fileExists(atPath: url.path(percentEncoded: false))
    }
}

// MARK: - SQLite

/// Reads every row of the fitsfiles table, newest first.
/// SQLite is a C library, so this uses its C functions directly: open the file, prepare the
/// SQL statement, step through the result rows, then finalize and close.
func readCatalog(at url: URL) throws -> [CatalogEntry] {
    var database: OpaquePointer?       // the open database file
    var statement: OpaquePointer?      // the prepared SQL statement
    defer {
        sqlite3_finalize(statement)    // always clean up, even after an error
        sqlite3_close(database)
    }

    // rowid is the row number SQLite gives every row (newest = highest).
    let sql = "SELECT rowid, target, telescope, kind, archive, download_link FROM fitsfiles ORDER BY rowid DESC"
    guard sqlite3_open_v2(url.path(percentEncoded: false), &database, SQLITE_OPEN_READONLY, nil) == SQLITE_OK,
          sqlite3_prepare_v2(database, sql, -1, &statement, nil) == SQLITE_OK else {
        throw CocoaError(.fileReadCorruptFile)
    }

    /// The text in column `column` of the current row ("" if it's empty).
    func text(_ column: Int32) -> String {
        sqlite3_column_text(statement, column).map { String(cString: $0) } ?? ""
    }

    var entries: [CatalogEntry] = []
    while sqlite3_step(statement) == SQLITE_ROW {      // one loop per row
        entries.append(CatalogEntry(id: sqlite3_column_int64(statement, 0), target: text(1), telescope: text(2),
                                    kind: text(3), archive: text(4), link: text(5)))
    }
    return entries
}
