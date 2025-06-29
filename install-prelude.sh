#!/bin/bash

./run.sh stop_daemon

rm -rf prelude

if [ ! -f installer.sh ]; then
    curl -L -o installer.sh https://github.com/bbatsov/prelude/raw/master/utils/installer.sh
    chmod +x installer.sh
fi

./installer.sh -d prelude # --verbose

./run.sh symlink_all symlinks.csv

./run.sh start_daemon