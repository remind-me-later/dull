#!/bin/bash



usage() {
    echo "Usage: $0 [-t] [-h] [-n NAME] <BAS_FILE>"
    echo "  -t       Trim whitespace from remarks"
    echo "  -h       Include header in binary output"
    echo "  -n NAME  Set program name in header (defaults to filename)"
    echo "  BAS_FILE Input BASIC file to compile"
    exit 1
}

TRIM_REMARKS=0
INCLUDE_HEADER=0
PROGRAM_NAME=""

# Parse options
while getopts "thn:" opt; do
    case $opt in
        t) TRIM_REMARKS=1 ;;
        h) INCLUDE_HEADER=1 ;;
        n) PROGRAM_NAME="$OPTARG" ;;
        *) usage ;;
    esac
done
shift $((OPTIND -1))

BAS_FILE="$1"
if [ -z "$BAS_FILE" ]; then
    echo "Error: BAS_FILE is required"
    usage
fi

HEX_FOLDER="test/hex"
BIN_FILE="$HEX_FOLDER/$(basename "${BAS_FILE%.*}").bin.tmp"
HEX_FILE="$HEX_FOLDER/$(basename "${BAS_FILE%.*}").hex.tmp"

if [ "$TRIM_REMARKS" -eq 1 ]; then
    REMARK_MODE="trim-whitespace"
else
    REMARK_MODE="keep-whole"
fi

# Build the cargo command
CARGO_ARGS="--remark-mode $REMARK_MODE -w $BAS_FILE -o $BIN_FILE"

if [ "$INCLUDE_HEADER" -eq 0 ]; then
    CARGO_ARGS="--no-header $CARGO_ARGS"
fi

if [ -n "$PROGRAM_NAME" ]; then
    CARGO_ARGS="$CARGO_ARGS -n $PROGRAM_NAME"
fi

cargo run -- $CARGO_ARGS
hexdump -C "$BIN_FILE" > "$HEX_FILE"