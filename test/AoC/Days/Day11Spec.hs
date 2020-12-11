module AoC.Days.Day11Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day11
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 11)
    parse
    (solveA, 2_494)
    (solveB, 2_306)
