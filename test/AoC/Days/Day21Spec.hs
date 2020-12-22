module AoC.Days.Day21Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day21
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 21)
    parse
    (solveA, ())
    (solveB, ())
