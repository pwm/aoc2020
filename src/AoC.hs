module AoC (solutions) where

import AoC.Days.Day01 qualified as Day01
import AoC.Lib.Day
import AoC.Lib.Solver
import AoC.Prelude

solutions :: [Day -> IO ()]
solutions =
  [ mkSolver Day01.parse Day01.solveA Day01.solveB
  ]
