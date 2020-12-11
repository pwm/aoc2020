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
    (mkDay 1)
    parse
    (solveA, 1_003_971)
    (solveB, 84_035_952)
