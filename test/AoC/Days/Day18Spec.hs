module AoC.Days.Day18Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day18
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 18)
    parse
    (solveA, 21347713555555)
    (solveB, 275011754427339)
