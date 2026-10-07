#!/bin/sh
#
# Xcode Cloud runs this right after it clones the repository.
#
# Since Xcode 26, the Metal Toolchain (the compiler for .metal shader files, here
# iFITS/AR/ARShaders.metal) isn't part of Xcode itself; it's a separate download. Your Mac has it
# (Xcode › Settings › Components), but a fresh Xcode Cloud machine may not, and the build then
# fails with "cannot execute tool 'metal' due to missing Metal Toolchain".

set -e

if xcrun --find metal >/dev/null 2>&1 && xcrun metal --version >/dev/null 2>&1; then
    echo "Metal Toolchain is already installed."
else
    echo "Downloading the Metal Toolchain…"
    xcodebuild -downloadComponent MetalToolchain
fi
