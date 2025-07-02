#!/bin/bash
set -e

mkdir -p ~/Library/LaunchAgents
cp org.gnu.emacs.daemon.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
