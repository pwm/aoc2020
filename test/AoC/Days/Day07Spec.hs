module AoC.Days.Day07Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day07
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 7)
    parse
    (solveA, 370)
    (solveB, 29547)
