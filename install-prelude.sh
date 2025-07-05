#!/bin/zsh

./run.sh stop_daemon

echo "[femacs] removing prelude"
rm -rf prelude

echo "[femacs] updating submodules"
git submodule update --init --recursive --remote

echo "[femacs] symlinking all"
./run.sh symlink_all symlinks.csv

echo "[femacs] starting daemon"
./run.sh start_daemon

echo "[femacs] done"
