module AoC.Days.Day04Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day04
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (UnsafeMkDay 4)
    parse
    (solveA, 200)
    (solveB, 116)
