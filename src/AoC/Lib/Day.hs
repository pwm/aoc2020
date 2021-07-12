module AoC.Lib.Day
  ( Day,
    mkDay,
    getDay,
    displayDay,
    displayDayFile,
    solutionsMapper,
  )
where

import AoC.Prelude
import Data.Map.Strict qualified as Map
import Text.Printf (printf)

newtype Day = UnsafeMkDay Int
  deriving stock (Show, Eq, Ord)

mkDay :: Int -> Either String Day
mkDay i
  | 0 <= i && i <= maxDayNum = Right $ UnsafeMkDay i
  | otherwise = Left $ "Valid days are in the [0.." <> show maxDayNum <> "] range"

getDay :: Day -> Int
getDay (UnsafeMkDay i) = i

displayDay :: Day -> String
displayDay = printf "%02d" . getDay

displayDayFile :: Day -> String
displayDayFile d = "Day" <> displayDay d <> ".txt"

solutionsMapper :: [sols] -> Map Day sols
solutionsMapper = Map.fromList . zip (fmap UnsafeMkDay [0 .. maxDayNum])

maxDayNum :: Int
maxDayNum = 25
