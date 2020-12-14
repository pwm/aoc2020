#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../"

today=$(date +%d)

cabal run fetch -- --day "$today"

cp -n src/AoC/Days/Day00.hs src/AoC/Days/Day"$today".hs
sed -i "s/Day00/Day$today/" src/AoC/Days/Day"$today".hs
echo "Created day file"

cp -n test/AoC/Days/Day00Spec.hs test/AoC/Days/Day"$today"Spec.hs
sed -i "s/Day00/Day$today/" test/AoC/Days/Day"$today"Spec.hs
sed -i "s/mkDay\ 0/mkDay\ $today/" test/AoC/Days/Day"$today"Spec.hs
echo "Created test file"

hpack
echo "Run hpack"
