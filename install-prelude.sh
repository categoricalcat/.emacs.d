#!/bin/bash

rm -rf prelude

curl -L -o installer.sh https://github.com/bbatsov/prelude/raw/master/utils/installer.sh

chmod +x installer.sh

./installer.sh -d prelude --verbose