module AoC.Days.Day06Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day06
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (UnsafeMkDay 6)
    parse
    (solveA, 7128)
    (solveB, 3640)
