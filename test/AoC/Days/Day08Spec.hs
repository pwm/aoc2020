module AoC.Days.Day08Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day08
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 8)
    parse
    (solveA, 1087)
    (solveB, 780)
