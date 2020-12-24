module AoC.Days.Day24Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day24
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 24)
    parse
    (solveA, 312)
    (solveB, 3_733)
