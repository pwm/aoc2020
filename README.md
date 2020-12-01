## (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ *Advent of Code 2020* (◕‿◕✿)

&nbsp;

Build env:

    $ direnv allow .

Build app:

    $ hpack && cabal build

Set session cookie for the fetcher:

    $ echo 'export AOC_SESSION=<my-aoc-2020-session-cookie>' > .envrc.private

Fetch day 1 input file:

    $ cabal run fetch -- --day 1

Solve day 1:

    $ cabal run solve -- --day 1

Build and use with nix:

    $ nix-build nix/release.nix
    $ result/bin/solve --day 1
