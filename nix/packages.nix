{ pkgs }: with pkgs;
let
  src-filter = import ./lib/src-filter.nix;

  # A view of the project's source code, with only the Haskell dependencies
  aoc2020-src = src-filter.filter {
    path = ../.;
    allow = [
      (src-filter.matchExt "hs")
      (src-filter.matchExt "txt")
      ../input
      ../exe
      ../src
      ../src/AoC
      ../src/AoC/Days
      ../src/AoC/Lib
      ../test
      ../test/AoC
      ../test/AoC/Days
      ../.hlint.yaml
      ../cabal.project
      ../hie.yaml
      ../aoc2020.cabal
      ../package.yaml
      ../LICENSE
      ../README.md
    ];
  };

  haskellPackages = haskell.packages.ghc8104.override {
    overrides =
      let
        generated = haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          aoc2020 = haskell.lib.overrideCabal hprev.aoc2020 (_drv: {
            src = aoc2020-src;
            postConfigure = ''
              ${hlint}/bin/hlint ${aoc2020-src}
            '';
          });
        };
      in
      lib.composeExtensions generated manual;
  };

  ghc = haskellPackages.ghc.withPackages (_ps:
    haskell.lib.getHaskellBuildInputs haskellPackages.aoc2020
  );
in
{
  exe = haskellPackages.aoc2020;

  shell = buildEnv {
    name = "aoc2020-env";
    paths = [
      ghc
      cabal-install
      cabal2nix
      hlint
      ormolu
      niv
      haskell-language-server
    ];
  };
}
