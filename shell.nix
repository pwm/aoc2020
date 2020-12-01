{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
in
with pkgs; mkShell {
  buildInputs = [
    aoc2020.shell
  ];
}
