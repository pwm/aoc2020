module AoC.Days.Day23Spec
  ( spec,
  )
where

import AoC.DayTester
import AoC.Lib.Day
import Test.Hspec

spec :: Spec
spec =
  dayTesterPending
    (mkDay 23)
    parsePending
    runPending
    runPending
