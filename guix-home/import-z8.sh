#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

# Invocation modes:
#   udiskie hook:  import-z8.sh {event} {id_label} {mount_path}
#   manual:        import-z8.sh         (uses sensible defaults)
EVENT="${1:-device_mounted}"
LABEL="${2:-NIKON Z 8}"
MOUNT="${3:-/run/media/${USER}/NIKON Z 8}"

EXPECTED_LABEL="NIKON Z 8"

# udiskie's event_hook fires for every event type; only act on a fresh mount.
if [[ "${EVENT}" != "device_mounted" ]]; then
  exit 0
fi

# Ignore mounts of any other device.
if [[ "${LABEL}" != "${EXPECTED_LABEL}" ]]; then
  exit 0
fi

SRC="${MOUNT}/DCIM/100NCZ_8"
DATE="$(date +%F)"
DEST="/mnt/backup/timmy/z8/${DATE}"

if [[ ! -d "$SRC" ]]; then
  echo "Card not mounted at $SRC — is it inserted?" >&2
  exit 1
fi

mkdir -p "$DEST"
# Import both video (.MOV) and Nikon raw (.NEF) files.
files=("$SRC"/*.MOV "$SRC"/*.NEF)
if (( ${#files[@]} == 0 )); then
  echo "No .MOV or .NEF files in $SRC" >&2
  exit 0
fi

cp "${files[@]}" "$DEST"/
echo "Copied ${#files[@]} files to $DEST"
