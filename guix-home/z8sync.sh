#!/usr/bin/env bash
set -euo pipefail

MOUNT="/run/media/${USER}/NIKON Z 8"

echo "==> Copying clips from card…"
if ! "${HOME}/.config/guix-home/import-z8.sh"; then
  echo "==> import-z8.sh failed; skipping unmount." >&2
  exit 1
fi

if mountpoint -q "$MOUNT"; then
  echo "==> Unmounting $MOUNT …"
  udiskie-umount "$MOUNT"
  echo "==> Safe to remove the card."
else
  echo "==> Nothing mounted at $MOUNT — nothing to unmount."
fi
