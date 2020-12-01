module AoC.Days.Day01 where

import AoC.Lib.SimpleParser
import AoC.Prelude

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = headOr 0 . solveFor 2

solveB :: [Int] -> Int
solveB = headOr 0 . solveFor 3

solveFor :: Int -> [Int] -> [Int]
solveFor n = fmap product . filter ((2020 ==) . sum) . choose n

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = fmap (x :) (choose (k - 1) xs) <> choose k xs
