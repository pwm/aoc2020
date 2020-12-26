#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../"

hpack
cabal2nix . > nix/packages/aoc2020.nix
direnv reload
