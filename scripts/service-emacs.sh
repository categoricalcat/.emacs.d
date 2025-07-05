#!/bin/zsh

service_dir=~/.config/systemd/user

function force_kill_emacs() {
  # lsof kill all 13245 port
  lsof -i :13245 | awk 'NR>1 {print $2}' | xargs kill -9
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
  emacsclient -c
}

function reload_attach_daemon() {
  stop_daemon
  start_daemon
  attach_daemon
}
