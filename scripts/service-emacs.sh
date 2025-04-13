#!/bin/zsh

service_dir=~/.config/systemd/user

function install_service_emacs() {
  echo "Installing emacs service"

  if [ -f $service_dir/emacs.service ]; then
    echo "emacs.service already exists"
    return
  fi

  if [ ! -f emacs.service ]; then
    echo "emacs.service source file not found"
    return
  fi

  cp emacs.service $service_dir/
  systemctl --user daemon-reload
  systemctl --user enable emacs.service
  systemctl --user start emacs.service

  systemctl --user status emacs.service
}

function uninstall_service_emacs() {
  echo "Uninstalling emacs service"

  if [ ! -f $service_dir/emacs.service ]; then
    echo "emacs.service not found"
    return
  fi

  systemctl --user stop emacs.service
  systemctl --user disable emacs.service
  rm $service_dir/emacs.service
  systemctl --user daemon-reload
}

function restart_service_emacs() {
  echo "Restarting emacs service"
  uninstall_service_emacs
  install_service_emacs
}
