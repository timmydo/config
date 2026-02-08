#!/bin/sh
# Fix Claude binary linking after updates on Guix.
# Usage: fix-claude.sh [claude-binary-path]
#
# If no path is given, finds the latest version under ~/.local/share/claude/versions/

set -eu

GLIBC_INTERP="/gnu/store/yj053cys0724p7vs9kir808x7fivz17m-glibc-2.41/lib/ld-linux-x86-64.so.2"
CLAUDE_VERSIONS_DIR="$HOME/.local/share/claude/versions"

if [ $# -ge 1 ]; then
    CLAUDE_BIN="$1"
else
    # Find the latest version directory
    CLAUDE_BIN=$(ls -1vd "$CLAUDE_VERSIONS_DIR"/*/ 2>/dev/null | tail -n1)
    if [ -z "$CLAUDE_BIN" ]; then
        echo "Error: no versions found in $CLAUDE_VERSIONS_DIR" >&2
        exit 1
    fi
    # Look for the claude binary inside the version directory
    if [ -f "${CLAUDE_BIN}claude" ]; then
        CLAUDE_BIN="${CLAUDE_BIN}claude"
    else
        # Fall back to treating the version dir path as the binary itself
        CLAUDE_BIN="${CLAUDE_BIN%/}"
    fi
fi

if [ ! -f "$CLAUDE_BIN" ]; then
    echo "Error: $CLAUDE_BIN does not exist" >&2
    exit 1
fi

echo "Patching: $CLAUDE_BIN"
echo "Setting interpreter to: $GLIBC_INTERP"
patchelf --set-interpreter "$GLIBC_INTERP" "$CLAUDE_BIN"
echo "Done."
