#!/usr/bin/env bash
# Helpers for managing the Emacs daemon.
# Source this file or call functions directly.

FEMACS_PORT="${EMACS_SERVER_PORT:-13245}"

force_kill() {
  echo "Force killing all Emacs processes..."
  lsof -i :"$FEMACS_PORT" 2>/dev/null | awk 'NR>1 {print $2}' | xargs -r kill -9 2>/dev/null
  pkill -i emacs 2>/dev/null; sleep 0.5
  pkill -9 -i emacs 2>/dev/null
  pkill -9 -i emacsclient 2>/dev/null
  rm -f ~/.emacs.d/server/* 2>/dev/null
  rm -f "/tmp/emacs$(id -u)/server" 2>/dev/null
  echo "All Emacs processes killed and server files cleaned up"
}

start_daemon() {
  echo "Starting emacs daemon"
  emacs --daemon --debug-init
}

stop_daemon() {
  echo "Stopping emacs daemon"
  emacsclient --eval '(kill-emacs)' 2>/dev/null || true
}

# Connect to a remote Emacs daemon over an SSH tunnel.
#   REMOTE_USER=yi REMOTE_HOST=10.0.0.5 attach_daemon
attach_daemon() {
  local user="${REMOTE_USER:-fufu}"
  local host="${REMOTE_HOST:-192.168.31.18}"
  local port="${REMOTE_PORT:-$FEMACS_PORT}"
  local server_file="${REMOTE_SERVER_FILE:-$HOME/.emacs.d/server/femacs}"

  cleanup() {
    [[ "${EMACSCLIENT:-}" ]] && kill "$EMACSCLIENT" 2>/dev/null
    [[ "${SSH_TUNNEL:-}" ]] && kill "$SSH_TUNNEL" 2>/dev/null
  }
  trap cleanup INT TERM EXIT

  ssh -N -L "${port}:localhost:${port}" "${user}@${host}" &
  SSH_TUNNEL=$!
  echo "Tunnel to ${user}@${host}:${port} (PID $SSH_TUNNEL)"
  sleep 0.3

  emacsclient -c -f "$server_file" &
  EMACSCLIENT=$!
  wait "$EMACSCLIENT"
}

reload_attach_daemon() {
  stop_daemon
  start_daemon
  attach_daemon
}

# Allow calling functions by name: ./service-emacs.sh start_daemon
if [[ "${1:-}" ]] && declare -f "$1" >/dev/null 2>&1; then
  "$@"
fi
