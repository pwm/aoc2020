module AoC.Days.Day09Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day09
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 9)
    parse
    (solveA, 1930745883)
    (solveB, 268878261)
