module AoC (solutions) where

import AoC.Days.Day01 qualified as Day01
import AoC.Days.Day02 qualified as Day02
import AoC.Days.Day03 qualified as Day03
import AoC.Days.Day04 qualified as Day04
import AoC.Lib.Day
import AoC.Lib.Solver
import AoC.Prelude

solutions :: [Day -> IO ()]
solutions =
  [ mkSolver Day01.parse Day01.solveA Day01.solveB,
    mkSolver Day02.parse Day02.solveA Day02.solveB,
    mkSolver Day03.parse Day03.solveA Day03.solveB,
    mkSolver Day04.parse Day04.solveA Day04.solveB
  ]
