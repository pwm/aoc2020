module AoC.Days.Day06 where

import AoC.Prelude
import Data.List (intersect, union)
import Data.List.Split (splitOn)

parse :: String -> Maybe [[String]]
parse = Just . fmap lines . splitOn "\n\n"

solveA :: [[String]] -> Int
solveA = sum . fmap (length . unions)

solveB :: [[String]] -> Int
solveB = sum . fmap (length . intersections)

unions :: [String] -> String
unions = foldr union []

intersections :: [String] -> String
intersections xs = foldr intersect (unions xs) xs
