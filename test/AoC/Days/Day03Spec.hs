module AoC.Days.Day03Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day03
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 3)
    parse
    (solveA, 203)
    (solveB, 3316272960)
