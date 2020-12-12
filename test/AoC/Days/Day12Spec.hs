module AoC.Days.Day12Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day12
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 12)
    parse
    (solveA, 1177)
    (solveB, 46530)
