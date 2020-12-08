module AoC.Days.Day01Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day01
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (UnsafeMkDay 1)
    parse
    (solveA, 1003971)
    (solveB, 84035952)
