//
//  CatalogViews.swift
//  iFITS
//
//  The catalog's screens:
//  - CatalogScreen: the tabs (Home, Images, Cubes, Search). On iPad the tab bar floats at the
//    top; on iPhone it sits at the bottom.
//  - CatalogHome: Recents in a sideways row, then every file.
//  - CatalogList / CatalogSearch: the Images, Cubes and Search tabs.
//  - CatalogDetail: one file, with Get / Open, Save to a Folder…, share, and its details.
//  - CatalogCard / CatalogRow / CatalogThumbnail: the small pieces they share.
//

import SwiftUI

// MARK: - Tabs

struct CatalogScreen: View {
    let store: CatalogStore
    var onClose: () -> Void            // back to the viewer
    var onOpen: (URL) -> Void          // open a downloaded file in the viewer

    var body: some View {
        TabView {
            Tab("Home", systemImage: "house") {
                tabStack { CatalogHome(store: store) }
            }
            Tab("Images", systemImage: "photo") {
                tabStack { CatalogList(entries: store.entries.filter { !$0.isCube }).navigationTitle("Images") }
            }
            Tab("Cubes", systemImage: "cube") {
                tabStack { CatalogList(entries: store.entries.filter(\.isCube)).navigationTitle("Cubes") }
            }
            Tab(role: .search) {
                tabStack { CatalogSearch(store: store) }
            }
        }
        .task { await store.load() }      // download the catalog when the screen appears
        .alert("Catalog", isPresented: Binding(get: { store.errorMessage != nil },
                                               set: { _ in store.errorMessage = nil })) {
            Button("OK") { }
        } message: {
            Text(store.errorMessage ?? "")
        }
    }

    /// Wraps a tab's content in navigation: the back-to-the-viewer button, and the detail page
    /// that opens when you tap an entry.
    private func tabStack<Content: View>(@ViewBuilder _ content: () -> Content) -> some View {
        NavigationStack {
            content()
                .toolbar {
                    ToolbarItem(placement: .topBarLeading) {
                        Button("Viewer", systemImage: "chevron.backward", action: onClose)
                    }
                }
                .navigationDestination(for: CatalogEntry.self) { entry in
                    CatalogDetail(entry: entry, store: store, onOpen: onOpen)
                }
        }
    }
}

// MARK: - Home tab

/// Recents (or, before you've looked at anything, the newest files) in a sideways row,
/// then a list of every file.
struct CatalogHome: View {
    let store: CatalogStore

    var body: some View {
        let recents = store.recentEntries
        let row = recents.isEmpty ? Array(store.entries.prefix(10)) : recents

        List {
            if !row.isEmpty {
                Section(recents.isEmpty ? "New in the Catalog" : "Recents") {
                    ScrollView(.horizontal) {
                        HStack(spacing: 14) {
                            ForEach(row) { entry in
                                NavigationLink(value: entry) { CatalogCard(entry: entry) }
                                    .buttonStyle(.plain)
                            }
                        }
                        .padding(.vertical, 4)
                    }
                    .scrollIndicators(.hidden)
                }
            }
            Section("All Files") {
                ForEach(store.entries) { entry in
                    NavigationLink(value: entry) { CatalogRow(entry: entry) }
                }
            }
        }
        .overlay {
            // Nothing to show yet: loading, or offline with no saved catalog.
            if store.entries.isEmpty {
                if store.isLoading {
                    ProgressView("Loading the catalog…")
                } else {
                    ContentUnavailableView {
                        Label("No Catalog Yet", systemImage: "books.vertical")
                    } description: {
                        Text("Connect to the internet to download the catalog.")
                    } actions: {
                        Button("Try Again") { Task { await store.load() } }
                    }
                }
            }
        }
        .navigationTitle("FITS Catalog")
    }
}

// MARK: - Images, Cubes and Search tabs

/// A list of entries; tapping one opens its detail page.
struct CatalogList: View {
    let entries: [CatalogEntry]

    var body: some View {
        List(entries) { entry in
            NavigationLink(value: entry) { CatalogRow(entry: entry) }
        }
        .overlay {
            if entries.isEmpty {
                ContentUnavailableView("Nothing Here Yet", systemImage: "sparkles")
            }
        }
    }
}

/// Search by object, telescope or archive.
struct CatalogSearch: View {
    let store: CatalogStore
    @State private var text = ""

    var body: some View {
        CatalogList(entries: text.isEmpty ? store.entries : store.entries.filter {
            "\($0.target) \($0.telescope) \($0.archive)".localizedCaseInsensitiveContains(text)
        })
        .searchable(text: $text, prompt: "Objects, telescopes, archives")
        .navigationTitle("Search")
    }
}

// MARK: - One entry

/// One file: a preview with Get / Open on it, then its details. On iPad (wide) they sit side
/// by side; on iPhone (narrow) they're stacked.
struct CatalogDetail: View {
    let entry: CatalogEntry
    let store: CatalogStore
    let onOpen: (URL) -> Void

    @Environment(\.horizontalSizeClass) private var sizeClass
    /// A downloaded file waiting to be moved to a folder you pick ("Save to a Folder…").
    @State private var fileToMove: URL?

    var body: some View {
        // Side by side when wide, stacked when narrow.
        let layout = sizeClass == .regular
            ? AnyLayout(HStackLayout(alignment: .top, spacing: 32))
            : AnyLayout(VStackLayout(alignment: .leading, spacing: 20))

        ScrollView {
            layout {
                CatalogThumbnail(entry: entry)
                    .aspectRatio(1, contentMode: .fit)
                    .frame(maxWidth: 460)
                    .overlay(alignment: .topTrailing) { getButton.padding() }
                details
                    .frame(maxWidth: .infinity, alignment: .leading)
            }
            .padding()
        }
        .navigationTitle(entry.target)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            // Share the download link (Messages, Mail, AirDrop…).
            if let url = entry.url { ShareLink(item: url) }
        }
        .onAppear { store.recordViewed(entry) }
        // The system's "move to a folder" picker, shown when fileToMove is set.
        .fileMover(isPresented: Binding(get: { fileToMove != nil }, set: { if !$0 { fileToMove = nil } }),
                   file: fileToMove) { _ in
            store.fileMoved()
        }
    }

    /// Open (already downloaded), a spinner (downloading), or Get (not downloaded).
    /// Tap Get to download into iFITS's Catalog folder; hold it for "Save to a Folder…".
    @ViewBuilder
    private var getButton: some View {
        if let file = store.localFile(for: entry) {
            Button("Open", systemImage: "scope") { onOpen(file) }
                .buttonStyle(.borderedProminent)
        } else if store.downloading.contains(entry.id) {
            ProgressView()
                .padding(10)
                .background(.regularMaterial, in: Capsule())
        } else {
            Menu {
                Button("Save to a Folder…", systemImage: "folder") {
                    Task { fileToMove = await store.download(entry) }
                }
            } label: {
                Label("Get", systemImage: "arrow.down.circle")
            } primaryAction: {
                Task { await store.download(entry) }
            }
            .buttonStyle(.borderedProminent)
        }
    }

    /// The entry's details from the catalog table.
    private var details: some View {
        VStack(alignment: .leading, spacing: 12) {
            Text(entry.target)
                .font(.largeTitle.bold())
            Text(entry.fileName)
                .font(.headline)
                .foregroundStyle(.secondary)
                .textSelection(.enabled)
            LabeledContent("Type", value: entry.kindName)
            LabeledContent("Telescope", value: entry.telescope)
            LabeledContent("Archive", value: entry.archive)
        }
    }
}

// MARK: - Shared pieces

/// A card in the Recents row: preview, target, type, archive.
struct CatalogCard: View {
    let entry: CatalogEntry

    var body: some View {
        HStack(spacing: 12) {
            CatalogThumbnail(entry: entry)
                .frame(width: 88, height: 88)
            VStack(alignment: .leading, spacing: 4) {
                Text(entry.target).font(.headline).lineLimit(2)
                Text(entry.kindName).foregroundStyle(.secondary)
                Text(entry.archive).foregroundStyle(.secondary)
            }
            .font(.subheadline)
            .frame(width: 150, alignment: .leading)
        }
        .padding(10)
        .background(.background.secondary, in: .rect(cornerRadius: 22))
    }
}

/// A row in the lists: preview, target, type and telescope.
struct CatalogRow: View {
    let entry: CatalogEntry

    var body: some View {
        HStack(spacing: 12) {
            CatalogThumbnail(entry: entry)
                .frame(width: 56, height: 56)
            VStack(alignment: .leading) {
                Text(entry.target).font(.headline)
                Text("\(entry.kindName), \(entry.telescope)")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }
        }
    }
}

/// A stand-in preview until the catalog has real preview images: a dark tile with a symbol
/// (sparkles for an image, a cube for a 3D cube).
struct CatalogThumbnail: View {
    let entry: CatalogEntry

    var body: some View {
        RoundedRectangle(cornerRadius: 16, style: .continuous)
            .fill(LinearGradient(colors: [entry.isCube ? .orange : .indigo, .black],
                                 startPoint: .topLeading, endPoint: .bottomTrailing))
            .overlay {
                Image(systemName: entry.isCube ? "cube.transparent" : "sparkles")
                    .resizable()
                    .scaledToFit()
                    .foregroundStyle(.white.opacity(0.85))
                    .frame(maxWidth: 110)
                    .padding(18)
            }
    }
}
