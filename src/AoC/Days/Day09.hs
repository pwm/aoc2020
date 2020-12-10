module AoC.Days.Day09 where

import AoC.Lib.SimpleParser
import AoC.Prelude
import Control.Arrow ((&&&))
import Prelude qualified as UNSAFE (head)

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = uncurry firstNotSumOf . splitAt 25

solveB :: [Int] -> Int
solveB xs = uncurry (+) . (minimum &&& maximum) . findSubsetSum 2 (solveA xs) $ xs

firstNotSumOf :: [Int] -> [Int] -> Int
firstNotSumOf p xs =
  let (x, t) = (UNSAFE.head xs, drop 1 xs)
   in if any ((x ==) . sum) $ choose 2 p
        then firstNotSumOf (drop 1 p <> [x]) t
        else x

findSubsetSum :: Int -> Int -> [Int] -> [Int]
findSubsetSum i x xs
  | i > length xs = []
  | otherwise =
    let ys = concat . filter ((x ==) . sum) . slicesOf i $ xs
     in if not (null ys) then ys else findSubsetSum (i + 1) x xs
