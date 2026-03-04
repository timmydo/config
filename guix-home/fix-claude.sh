#!/bin/sh
# Fix Claude binary linking after updates on Guix.
# Usage: fix-claude.sh [claude-binary-path]
#
# If no path is given, finds the newest valid executable under
# ~/.local/share/claude/versions/

set -eu

GLIBC_INTERP="/gnu/store/yj053cys0724p7vs9kir808x7fivz17m-glibc-2.41/lib/ld-linux-x86-64.so.2"
CLAUDE_VERSIONS_DIR="$HOME/.local/share/claude/versions"

find_latest_valid_claude_bin() {
    # Newest version first, then pick first executable non-empty file.
    ls -1 "$CLAUDE_VERSIONS_DIR" 2>/dev/null | sort -Vr | while IFS= read -r version; do
        candidate="$CLAUDE_VERSIONS_DIR/$version"
        [ -f "$candidate" ] || continue
        [ -x "$candidate" ] || continue
        [ -s "$candidate" ] || continue
        echo "$candidate"
        return 0
    done
    return 1
}

if [ $# -ge 1 ]; then
    CLAUDE_BIN="$1"
else
    CLAUDE_BIN="$(find_latest_valid_claude_bin || true)"
    if [ -z "${CLAUDE_BIN}" ]; then
        echo "Error: no valid executable files found in $CLAUDE_VERSIONS_DIR" >&2
        exit 1
    fi
fi

if [ ! -f "$CLAUDE_BIN" ]; then
    echo "Error: $CLAUDE_BIN does not exist" >&2
    exit 1
fi

if ! file -b "$CLAUDE_BIN" | grep -q '^ELF '; then
    echo "Error: $CLAUDE_BIN is not an ELF binary" >&2
    exit 1
fi

echo "Patching: $CLAUDE_BIN"
echo "Setting interpreter to: $GLIBC_INTERP"
patchelf --set-interpreter "$GLIBC_INTERP" "$CLAUDE_BIN"
echo "Done."
