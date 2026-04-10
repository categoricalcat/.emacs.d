#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

command -v emacs >/dev/null || { echo "error: emacs not found"; exit 1; }
command -v git   >/dev/null || { echo "error: git not found";   exit 1; }

EMACS_VER=$(emacs --version | head -1 | grep -oP '\d+\.\d+')
echo "[femacs] emacs $EMACS_VER detected"

echo "[femacs] initializing submodules"
make submodule

echo "[femacs] linking config"
make link

echo "[femacs] done — start emacs or run: make start"
