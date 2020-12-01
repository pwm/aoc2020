{ system ? builtins.currentSystem }:
let
  pkgs = import ./. { inherit system; };
in
pkgs.aoc2020
