module AoC.Days.Day14Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day14
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 14)
    parse
    (solveA, 2_346_881_602_152)
    (solveB, 3_885_232_834_169)
