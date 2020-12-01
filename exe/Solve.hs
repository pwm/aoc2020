module Main (main) where

import AoC
import AoC.Lib.ArgParser
import AoC.Lib.Solver
import AoC.Prelude

main :: IO ()
main = execArgParser opts >>= either error (solveDay solutions)
