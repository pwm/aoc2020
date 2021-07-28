module AoC.Days.Day20Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day20
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 20)
    parse
    (solveA, 12_519_494_280_967)
    (solveB, 2_442)
