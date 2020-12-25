module AoC.Days.Day25Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day25
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 25)
    parse
    (solveA, 12_181_021)
    (solveB, ())
