#!/bin/bash
BAS_FILE="$1"
BIN_FILE="test_bin/My$(basename "${BAS_FILE%.*}").bin"
HEX_FILE="test_bin/My$(basename "${BAS_FILE%.*}").hex"

cargo run -- --no-header -w "$BAS_FILE" -o "$BIN_FILE"
hexdump -C "$BIN_FILE" > "$HEX_FILE"