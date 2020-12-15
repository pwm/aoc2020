module AoC.Days.Day15Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day15
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 15)
    parse
    (solveA, 755)
    (solveB, 11_962)
