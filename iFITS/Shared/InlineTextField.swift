//
//  InlineTextField.swift
//  iFITS Start
//
//  Text box that is only editable while tapped.
//

import SwiftUI

/// Text box that only applies its text when you press Return or leave it (like NumberField),
/// and only becomes editable when tapped, so it never holds the keyboard in the background.
struct InlineTextField: View {
    let text: String
    var monospaced = false
    var alignment: TextAlignment = .leading
    var onCommit: (String) -> Void

    @State private var draft = ""
    @State private var isEditing = false
    @FocusState private var focused: Bool

    private var font: Font { monospaced ? .body.monospacedDigit() : .body }

    var body: some View {
        Group {
            if isEditing {
                TextField("", text: $draft)
                    .autocorrectionDisabled()
                    .textInputAutocapitalization(.never)
                    .multilineTextAlignment(alignment)
                    .font(font)
                    .textFieldStyle(.roundedBorder)
                    .focused($focused)
                    .onSubmit { finishEditing() }
                    .onChange(of: focused) { _, isFocused in
                        if !isFocused { finishEditing() }
                    }
                    .onAppear {
                        draft = text
                        DispatchQueue.main.async { focused = true }
                    }
            } else {
                Button {
                    isEditing = true
                } label: {
                    Text(text.isEmpty ? " " : text)
                        .font(font)
                        .lineLimit(1)
                        .frame(maxWidth: .infinity, alignment: alignment == .trailing ? .trailing : .leading)
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
                .accessibilityLabel("\(text). Tap to edit.")
            }
        }
    }

    private func finishEditing() {
        guard isEditing else { return }
        if draft != text { onCommit(draft) }
        isEditing = false
        focused = false
        NotificationCenter.default.post(name: .iFITSRestoreImageFocus, object: nil)
    }
}
