module AoC.Days.Day14Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day14
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 14)
    parse
    (solveA, 2346881602152)
    (solveB, 3885232834169)
