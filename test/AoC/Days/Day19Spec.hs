module AoC.Days.Day19Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day19
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 19)
    parse
    (solveA, 205)
    (solveB, 329)
