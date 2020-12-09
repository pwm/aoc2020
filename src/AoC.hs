module AoC (solutions) where

import AoC.Days.Day00 qualified as Day00
import AoC.Days.Day01 qualified as Day01
import AoC.Days.Day02 qualified as Day02
import AoC.Days.Day03 qualified as Day03
import AoC.Days.Day04 qualified as Day04
import AoC.Days.Day05 qualified as Day05
import AoC.Days.Day06 qualified as Day06
import AoC.Days.Day07 qualified as Day07
import AoC.Days.Day08 qualified as Day08
import AoC.Days.Day09 qualified as Day09
import AoC.Lib.Day
import AoC.Lib.Solver
import AoC.Prelude

solutions :: Map Day (Day -> IO ())
solutions =
  solutionsMapper
    [ mkSolver Day00.parse Day00.solveA Day00.solveB,
      mkSolver Day01.parse Day01.solveA Day01.solveB,
      mkSolver Day02.parse Day02.solveA Day02.solveB,
      mkSolver Day03.parse Day03.solveA Day03.solveB,
      mkSolver Day04.parse Day04.solveA Day04.solveB,
      mkSolver Day05.parse Day05.solveA Day05.solveB,
      mkSolver Day06.parse Day06.solveA Day06.solveB,
      mkSolver Day07.parse Day07.solveA Day07.solveB,
      mkSolver Day08.parse Day08.solveA Day08.solveB,
      mkSolver Day09.parse Day09.solveA Day09.solveB
    ]
