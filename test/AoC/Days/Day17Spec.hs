module AoC.Days.Day17Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day17
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 17)
    parse
    (solveA, 215)
    (solveB, 1_728)
