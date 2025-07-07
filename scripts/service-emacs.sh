#!/bin/zsh

function force_kill() {
  echo "Force killing all Emacs processes..."

  lsof -i :13245 2>/dev/null | awk 'NR>1 {print $2}' | xargs -r kill -9 2>/dev/null

  pkill -i emacs 2>/dev/null

  sleep 0.5

  pkill -9 -i emacs 2>/dev/null

  pkill -9 -i emacsclient 2>/dev/null

  rm -f ~/.emacs.d/server/* 2>/dev/null
  rm -f /tmp/emacs$(id -u)/server 2>/dev/null

  echo "All Emacs processes killed and server files cleaned up"
}

function start_daemon() {
  echo "Starting emacs service"

  emacs --daemon --debug-init
}

function stop_daemon() {
  echo "Stopping emacs service"

  emacsclient --eval '(kill-emacs)'
}

function attach_daemon() {
  USER="fares"
  HOST="192.168.32.21"
  PORT="13245"
  SERVER_FILE="$HOME/.emacs.d/server/remote-emacs-server"

  cleanup() {
    [[ $EMACSCLIENT ]] && kill "$EMACSCLIENT"
    [[ $SSH_TUNNEL ]] && kill "$SSH_TUNNEL"
  }
  trap cleanup INT TERM EXIT

  ssh -N -L "${PORT}:localhost:${PORT}" "${USER}@${HOST}" &
  SSH_TUNNEL=$!
  echo "Attaching to remote emacs server at ${USER}@${HOST}:${PORT} with server file ${SERVER_FILE} and tunnel PID ${SSH_TUNNEL}"
  sleep 0.3

  emacsclient -c -f "$SERVER_FILE" &
  EMACSCLIENT=$!

  wait "$EMACSCLIENT"
}

function reload_attach_daemon() {
  stop_daemon
  start_daemon
  attach_daemon
}
