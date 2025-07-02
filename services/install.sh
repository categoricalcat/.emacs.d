#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    exec "$SCRIPT_DIR/install-linux.sh"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    exec "$SCRIPT_DIR/install-macos.sh"
else
    echo "Unsupported OS: $OSTYPE" >&2
    exit 1
fi
