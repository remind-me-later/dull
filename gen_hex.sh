#!/bin/bash

TRIM_REMARKS=0

# Parse options
while getopts "t" opt; do
    case $opt in
        t) TRIM_REMARKS=1 ;;
        *) ;;
    esac
done
shift $((OPTIND -1))

BAS_FILE="$1"
BIN_FILE="test_bin/My$(basename "${BAS_FILE%.*}").bin"
HEX_FILE="test_bin/My$(basename "${BAS_FILE%.*}").hex"

if [ "$TRIM_REMARKS" -eq 1 ]; then
    REMARK_MODE="trim-whitespace"
else
    REMARK_MODE="keep-whole"
fi

cargo run -- --no-header --remark-mode "$REMARK_MODE" -w "$BAS_FILE" -o "$BIN_FILE"
hexdump -C "$BIN_FILE" > "$HEX_FILE"