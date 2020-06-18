#!/bin/sh

# All credits go to Relude https://github.com/reazen/relude/blob/761f6471c9b0b79925ab358b4f12e3d20fa5cbd3/.github/workflows/release.yml

./node_modules/.bin/bsdoc support-files
./node_modules/.bin/bsdoc build -v --debug Noir

mv ./docs/Noir/Noir/index.html ./docs

sed -i -E "s/\([a-zA-Z_]*\) = \([a-zA-Z_-]*\)/\1 = <a href=\"\/Noir\/\2\">\2<\/a>/g" "./docs/index.html"
