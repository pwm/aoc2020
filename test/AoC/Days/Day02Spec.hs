module AoC.Days.Day02Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day02
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 2)
    parse
    (solveA, 524)
    (solveB, 485)
