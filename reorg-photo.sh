#!/bin/bash
set -euo pipefail

PHOTO_ROOT=${PHOTO_ROOT:-by-date}
for file in "$@";do
    HASH=$(xxh128sum -q "$file"|cut -d ' ' -f1)
    EXT=${file##*.}
    YEAR=unknown
    MONTH=unknown
    DNAME=$(dirname "$file")
    DATETIME=unknown

    #if [[ "$DNAME" =~ .*([0-9][0-9][0-9][0-9])-([0-9][0-9])-[0-9][0-9].* ]]; then
    #   YEAR=${BASH_REMATCH[1]}
    #   MONTH=${BASH_REMATCH[2]}
    #fi

    if [[ $YEAR == "unknown" ]]; then
	DATETIME=$(exiftool -DateTimeOriginal -d %Y-%m-%d -j "$file" 2>/dev/null| jq -r '.[0].DateTimeOriginal')
	if [[ "$DATETIME" =~ .*([0-9][0-9][0-9][0-9])-([0-9][0-9])-[0-9][0-9].* ]]; then
	    YEAR=${BASH_REMATCH[1]}
	    MONTH=${BASH_REMATCH[2]}
	fi
    fi

    if [[ $YEAR == "unknown" || $MONTH == "unknown" ]]; then
	echo "Unresolved date $YEAR/$MONTH: $file"
    else
	mkdir -p "${PHOTO_ROOT}/${YEAR}/${MONTH}"
	ln -v "$file" "${PHOTO_ROOT}/${YEAR}/${MONTH}/${HASH}.${EXT}"
    fi
    
done
