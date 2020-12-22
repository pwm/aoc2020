module AoC.Days.Day22Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day22
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 22)
    parse
    (solveA, 32_815)
    (solveB, 30_695)
