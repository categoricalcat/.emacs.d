#!/bin/bash
set -e

mkdir -p ~/.config/systemd/user

if [[ -f ~/.config/systemd/user/emacs-daemon.service ]]; then
  systemctl --user stop emacs-daemon.service
  systemctl --user disable emacs-daemon.service
  rm -f ~/.config/systemd/user/emacs-daemon.service
  systemctl --user daemon-reload
fi

cp emacs-daemon.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable emacs-daemon.service
systemctl --user start emacs-daemon.service
