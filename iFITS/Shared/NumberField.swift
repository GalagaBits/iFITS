//
//  NumberField.swift
//  iFITS Start
//
//  Number box that applies on Return, plus the focus-restore notification.
//

import SwiftUI

extension Notification.Name {
    /// Posted when a text box finishes editing, so ContentView can re-focus the image.
    static let iFITSRestoreImageFocus = Notification.Name("iFITSRestoreImageFocus")
}

/// Number box that only applies its value when you press Return or leave the field,
/// so the image doesn't re-render on every keystroke.
///
/// It shows the value as plain text and only becomes an editable text field when tapped.
/// That way it can never sit "focused" in the background holding the text cursor (and the
/// arrow keys) when you're not actually typing in it.
struct NumberField: View {
    let value: Double
    var onCommit: (Double) -> Void

    @State private var text = ""
    @State private var isEditing = false
    @FocusState private var focused: Bool

    var body: some View {
        Group {
            if isEditing {
                TextField("Value", text: $text)
                    .keyboardType(.numbersAndPunctuation)
                    .autocorrectionDisabled()
                    .textInputAutocapitalization(.never)
                    .multilineTextAlignment(.trailing)
                    .font(.body.monospacedDigit())
                    .textFieldStyle(.roundedBorder)
                    .focused($focused)
                    .onSubmit { finishEditing() }
                    .onChange(of: focused) { _, isFocused in
                        if !isFocused { finishEditing() }
                    }
                    .onAppear {
                        text = NumberField.format(value)
                        DispatchQueue.main.async { focused = true }
                    }
            } else {
                Button {
                    isEditing = true
                } label: {
                    Text(NumberField.format(value))
                        .font(.body.monospacedDigit())
                        .lineLimit(1)
                        .frame(maxWidth: .infinity, alignment: .trailing)
                        .padding(.horizontal, 7)
                        .padding(.vertical, 5)
                        .background(Color(uiColor: .systemBackground),
                                    in: RoundedRectangle(cornerRadius: 5, style: .continuous))
                        .overlay(RoundedRectangle(cornerRadius: 5, style: .continuous)
                            .stroke(Color(uiColor: .separator), lineWidth: 0.5))
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .focusable(false)
                .accessibilityLabel("Value \(NumberField.format(value)). Tap to edit.")
            }
        }
    }

    private func finishEditing() {
        guard isEditing else { return }
        let cleaned = text.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: ",", with: ".")
        if let v = Double(cleaned), v.isFinite, v != value {
            onCommit(v)
        }
        isEditing = false
        focused = false
        // Let the window give keyboard focus back to the image (arrow keys, menu bar).
        NotificationCenter.default.post(name: .iFITSRestoreImageFocus, object: nil)
    }

    static func format(_ v: Double) -> String {
        String(format: "%.6g", v)
    }
}
