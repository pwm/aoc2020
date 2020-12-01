module Main (main) where

import AoC.Lib.ArgParser
import AoC.Lib.Fetcher
import AoC.Prelude

main :: IO ()
main = execArgParser opts >>= either error (fetchDay "2020")
