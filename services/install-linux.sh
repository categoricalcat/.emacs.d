#!/bin/bash
set -e

mkdir -p ~/.config/systemd/user
cp emacs-daemon.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable emacs-daemon.service
systemctl --user start emacs-daemon.service
