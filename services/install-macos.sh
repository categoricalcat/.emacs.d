#!/bin/bash
set -e

# Create LaunchAgents directory if it doesn't exist
mkdir -p ~/Library/LaunchAgents

# if cp already exists, remove it
if [[ -f ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist ]]; then
  echo "Unloading Emacs daemon"
  launchctl unload ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
  rm -f ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
fi

echo "Copying Emacs daemon plist"
cp ~/.emacs.d/services/org.gnu.emacs.daemon.plist ~/Library/LaunchAgents/

echo "Loading Emacs daemon"
launchctl load ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
