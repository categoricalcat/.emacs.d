#!/usr/bin/env bash
set -euo pipefail

# Override with env vars or pass as arguments:
#   REMOTE_USER=yi REMOTE_HOST=10.0.0.5 ./connect-remote-emacs.sh /some/file
USER="${REMOTE_USER:-fufu}"
HOST="${REMOTE_HOST:-192.168.31.18}"
PORT="${REMOTE_PORT:-13245}"
SERVER_FILE="${REMOTE_SERVER_FILE:-$HOME/.emacs.d/server/femacs}"
FILE_TO_EDIT="${1:-.}"

cleanup() { [[ "${SSH_PID:-}" ]] && kill "$SSH_PID" 2>/dev/null; }
trap cleanup INT TERM EXIT

echo "Creating SSH tunnel to ${USER}@${HOST}:${PORT}..."
ssh -f -N -L "${PORT}:localhost:${PORT}" "${USER}@${HOST}"
SSH_PID=$(pgrep -f "ssh -f -N -L ${PORT}:localhost:${PORT}" | head -1)

if [[ -z "${SSH_PID:-}" ]]; then
  echo "error: SSH tunnel process not found" >&2
  exit 1
fi

echo "SSH tunnel established (PID: $SSH_PID)"
sleep 1

echo "Connecting to remote Emacs server..."
emacsclient -c -f "$SERVER_FILE" "$FILE_TO_EDIT"
