module AoC.Days.Day03 where

import AoC.Lib.Grid
import AoC.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Grid
parse = parseGrid (Just . (== '#'))

solveA :: Grid -> Int
solveA m = numTrees m leg1

solveB :: Grid -> Int
solveB m = product $ numTrees m <$> [leg1, leg2, leg3, leg4, leg5]

type Grid = GridOf Bool

numTrees :: Grid -> (Pos -> Pos) -> Int
numTrees m leg =
  length
    . Map.filter identity
    . Map.restrictKeys m
    . Set.fromList
    . cyclicLegs leg m
    $ (0, 0)

cyclicLegs :: (Pos -> Pos) -> Grid -> Pos -> [(Int, Int)]
cyclicLegs leg m =
  takeWhile ((<= vMax) . fst) . iterate (second (`mod` (hMax + 1)) . leg)
  where
    (vMax, hMax) = fst $ Map.findMax m

leg1, leg2, leg3, leg4, leg5 :: Pos -> Pos
leg1 = flip move4 [R, R, R, D]
leg2 = flip move4 [R, D]
leg3 = flip move4 [R, R, R, R, R, D]
leg4 = flip move4 [R, R, R, R, R, R, R, D]
leg5 = flip move4 [R, D, D]
