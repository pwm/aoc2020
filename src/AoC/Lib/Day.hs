module AoC.Lib.Day where

import AoC.Prelude
import Text.Printf (printf)

newtype Day = UnsafeMkDay Int
  deriving stock (Show, Eq, Ord)

getDay :: Day -> Int
getDay (UnsafeMkDay i) = i

mkDay :: Int -> Either String Day
mkDay i
  | 1 <= i && i <= 25 = Right $ UnsafeMkDay i
  | otherwise = Left "Valid days are in the [1..25] range"

displayDay :: Day -> String
displayDay = printf "%02d" . getDay
