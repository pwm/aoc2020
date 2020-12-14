module AoC.Days.Day13Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day13
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 13)
    parse
    (solveA, 2215)
    (solveB, ())
