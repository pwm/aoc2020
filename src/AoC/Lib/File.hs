module AoC.Lib.File where

import AoC.Lib.Day
import AoC.Prelude

loadInputFile :: Day -> IO String
loadInputFile day = do
  f <- getDataFileName $ "input/" <> displayDayFile day
  readFile f
