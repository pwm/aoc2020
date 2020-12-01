#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../"

today=$(date +%d)
cp -n src/AoC/Days/Day00.hs src/AoC/Days/Day"$today".hs
sed -i "s/Day00/Day$today/" src/AoC/Days/Day"$today".hs
