{ sources }:
[
  (_final: prev: {
    aoc2020 = (import ./packages.nix { pkgs = prev; });
  })
]
