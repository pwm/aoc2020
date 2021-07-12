module AoC.Days.Day20Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTesterPending
    (mkDay 20)
    parsePending
    runPending
    runPending
