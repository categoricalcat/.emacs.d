#!/bin/zsh

service_dir=~/.config/systemd/user

function install_service_emacs() {
  echo "Installing emacs service"

  emacs --daemon --debug-init
}

function uninstall_service_emacs() {
  echo "Uninstalling emacs service"

  emacsclient --eval '(kill-emacs)'
}

function reload_attach_emacs() {
  emacsclient --eval '(kill-emacs)'
  emacs --daemon --debug-init
  emacsclient -c
}
