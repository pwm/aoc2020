module AoC.Days.Day10Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day10
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 10)
    parse
    (solveA, 2_343)
    (solveB, 31_581_162_962_944)
