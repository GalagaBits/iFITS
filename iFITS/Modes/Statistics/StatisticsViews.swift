//
//  StatisticsViews.swift
//  iFITS Start
//
//  The statistics box (top right) and the S-mode dock (statistics and SNR pages).
//

import SwiftUI

struct StatisticRow: Identifiable {
    let label: String
    let value: String
    let unit: String
    var id: String { label }
}

/// Statistic | value | unit, in a monospaced table.
struct StatisticsGrid: View {
    let stats: RegionStatistics?
    let unit: String
    var showHeader = false

    var body: some View {
        Grid(alignment: .leading, horizontalSpacing: 12, verticalSpacing: 4) {
            if showHeader {
                GridRow {
                    Text("Statistic")
                    Text("Value").gridCellColumns(2)
                }
                .font(.subheadline.weight(.semibold))
                Divider()
            }
            ForEach(Self.rows(stats, unit: unit)) { row in
                GridRow {
                    Text(row.label)
                        .foregroundStyle(.secondary)
                    Text(row.value)
                        .gridColumnAlignment(.trailing)
                    Text(row.unit)
                        .foregroundStyle(.secondary)
                }
                .font(.system(.callout, design: .monospaced))
            }
        }
    }

    static func rows(_ stats: RegionStatistics?, unit: String) -> [StatisticRow] {
        func text(_ v: Double?) -> String {
            guard let v else { return "—" }
            return v.isNaN ? "NaN" : String(format: "%.6g", v)
        }
        return [
            StatisticRow(label: "NumPixels", value: stats.map { "\($0.count)" } ?? "—", unit: "pixel(s)"),
            StatisticRow(label: "Sum", value: text(stats?.sum), unit: unit),
            StatisticRow(label: "Mean", value: text(stats?.mean), unit: unit),
            StatisticRow(label: "StdDev", value: text(stats?.stdDev), unit: unit),
            StatisticRow(label: "Min", value: text(stats?.min), unit: unit),
            StatisticRow(label: "Max", value: text(stats?.max), unit: unit),
            StatisticRow(label: "RMS", value: text(stats?.rms), unit: unit)
        ]
    }
}

/// Top-right glass box with the statistics (shown in every mode until closed;
/// tapping S brings it back).
struct StatisticsBox: View {
    let regionName: String
    let stats: RegionStatistics?
    let unit: String
    var onClose: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 6) {
                Image(systemName: "sum")
                    .foregroundStyle(.secondary)
                Text("Statistics")
                    .font(.subheadline.weight(.semibold))
                Text(regionName)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                Spacer(minLength: 16)
                Button(action: onClose) {
                    Image(systemName: "xmark")
                        .font(.caption.weight(.bold))
                        .foregroundStyle(.secondary)
                        .frame(width: 26, height: 26)
                        .background(.quaternary, in: Circle())
                        .contentShape(Circle())
                }
                .buttonStyle(.plain)
                .hoverEffect(.highlight)
                .accessibilityLabel("Close statistics")
            }
            StatisticsGrid(stats: stats, unit: unit)
        }
        .padding(14)
        .fixedSize()
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 20, style: .continuous))
    }
}

/// S mode: two pages you swipe between, like the Home Screen. Page 1 picks the region to measure
/// and shows its statistics; page 2 is the SNR page (noise, cutoff, Calculate SNR).
struct StatisticsDockPanel<SNRPage: View>: View {
    @Bindable var store: RegionStore
    @Binding var expanded: Bool
    /// 0 = statistics, 1 = SNR. Kept by ContentView, so it's remembered across mode switches.
    @Binding var page: Int?
    let stats: RegionStatistics?
    let unit: String
    let glassNamespace: Namespace.ID
    private let snrPage: SNRPage

    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    init(store: RegionStore, expanded: Binding<Bool>, page: Binding<Int?>, stats: RegionStatistics?,
         unit: String, glassNamespace: Namespace.ID, @ViewBuilder snrPage: () -> SNRPage) {
        _store = Bindable(wrappedValue: store)
        _expanded = expanded
        _page = page
        self.stats = stats
        self.unit = unit
        self.glassNamespace = glassNamespace
        self.snrPage = snrPage()
    }

    private var regionName: String { store.statsRegion?.name ?? "Entire Image" }
    private var currentPage: Int { page ?? 0 }

    var body: some View {
        CollapsibleDock(expanded: $expanded, glassNamespace: glassNamespace) {
            VStack(alignment: .leading, spacing: 10) {
                HStack(spacing: 12) {
                    pageTab("Statistics", systemImage: "sum", page: 0)
                    pageTab("SNR", systemImage: "waveform.path.ecg", page: 1)
                    Spacer(minLength: 0)
                    if currentPage == 0 {
                        Text("Region")
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
                        regionMenu
                    }
                    DockCollapseButton(expanded: $expanded)
                }
                Divider()

                // Swipe left / right between the pages; they snap into place one at a time.
                ScrollViewReader { proxy in
                    ScrollView(.horizontal) {
                        HStack(alignment: .top, spacing: 0) {
                            statisticsPage
                                .padding(.horizontal, 2)
                                .containerRelativeFrame(.horizontal)
                                .id(0)
                            snrPage
                                .padding(.horizontal, 2)
                                .containerRelativeFrame(.horizontal)
                                .id(1)
                        }
                        .scrollTargetLayout()
                    }
                    .scrollTargetBehavior(.paging)
                    .scrollPosition(id: $page)
                    .scrollIndicators(.hidden)
                    // Coming back to S mode (or expanding the dock) shows the page you left it on.
                    .onAppear {
                        guard currentPage != 0 else { return }
                        let target = currentPage
                        DispatchQueue.main.async { proxy.scrollTo(target, anchor: .leading) }
                    }
                }

                pageDots
            }
        } mini: {
            HStack(spacing: 10) {
                Image(systemName: "sum")
                Text(regionName)
                    .font(.subheadline.weight(.semibold))
                if let mean = stats?.mean, mean.isFinite {
                    Text("Mean " + String(format: "%.4g", mean))
                        .font(.subheadline.monospacedDigit())
                        .foregroundStyle(.secondary)
                }
                Image(systemName: "chevron.up")
                    .font(.caption.weight(.bold))
            }
        }
    }

    private var statisticsPage: some View {
        VStack(alignment: .leading, spacing: 10) {
            StatisticsGrid(stats: stats, unit: unit, showHeader: true)
                .frame(maxWidth: .infinity, alignment: .leading)
            if store.statsCandidates.isEmpty {
                Text("Draw an ellipse, rectangle or point in R mode to measure part of the image.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }

    /// Page title you can tap to go to that page.
    private func pageTab(_ title: String, systemImage: String, page target: Int) -> some View {
        Button {
            withAnimation(reduceMotion ? nil : Animation.smooth(duration: 0.35)) { page = target }
        } label: {
            Label(title, systemImage: systemImage)
                .font(.headline)
                .lineLimit(1)
                .fixedSize()
                .foregroundStyle(currentPage == target ? AnyShapeStyle(.primary) : AnyShapeStyle(.tertiary))
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .hoverEffect(.highlight)
        .accessibilityAddTraits(currentPage == target ? .isSelected : [])
    }

    /// Two dots under the pages, like the Home Screen's.
    private var pageDots: some View {
        HStack(spacing: 8) {
            ForEach(0..<2, id: \.self) { i in
                Circle()
                    .fill(currentPage == i ? AnyShapeStyle(.primary) : AnyShapeStyle(.tertiary))
                    .frame(width: 7, height: 7)
                    .onTapGesture {
                        withAnimation(reduceMotion ? nil : Animation.smooth(duration: 0.35)) { page = i }
                    }
            }
        }
        .frame(maxWidth: .infinity)
        .animation(.snappy(duration: 0.2), value: currentPage)
        .accessibilityHidden(true)
    }

    private var regionMenu: some View {
        Menu {
            Button {
                store.statsRegionID = nil
            } label: {
                if store.statsRegion == nil {
                    Label("Entire Image", systemImage: "checkmark")
                } else {
                    Text("Entire Image")
                }
            }
            if !store.statsCandidates.isEmpty {
                Divider()
            }
            ForEach(store.statsCandidates) { region in
                Button {
                    store.select(region.id)
                } label: {
                    if region.id == store.statsRegion?.id {
                        Label(region.name, systemImage: "checkmark")
                    } else {
                        Text(region.name)
                    }
                }
            }
        } label: {
            DockMenuLabel(text: regionName)
        }
        .menuOrder(.fixed)
    }
}
