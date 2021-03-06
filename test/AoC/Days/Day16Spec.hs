module AoC.Days.Day16Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Days.Day16
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 16)
    parse
    (solveA, 25_895)
    (solveB, 5_865_723_727_753)
