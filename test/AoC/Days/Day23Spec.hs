module AoC.Days.Day23Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day23
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 23)
    parse
    (solveA, ())
    (solveB, ())
