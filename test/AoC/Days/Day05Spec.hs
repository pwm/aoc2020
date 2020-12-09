module AoC.Days.Day05Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day05
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 5)
    parse
    (solveA, 944)
    (solveB, 554)
