module AoC.Days.Day15 where

import AoC.Lib.SimpleParser
import AoC.Prelude hiding (head)
import Data.HashTable.Class qualified as MHM
import Data.HashTable.ST.Linear (HashTable)
import Data.Massiv.Array qualified as A
import Prelude (last)

parse :: String -> Maybe [Int]
parse = stringToIntsSepBy ","

solveA :: [Int] -> Int
solveA = solveForMAU 2020

solveB :: [Int] -> Int
solveB = solveForMAU 30_000_000

-- From @ethercrow - Mut Array Unboxed ~0.5sec
solveForMAU :: Int -> [Int] -> Int
solveForMAU limit xs = runST $ do
  m <- A.new @A.U (A.Sz1 $ limit + 2)
  forM_ (zip xs [1 ..]) $ uncurry (A.writeM m)
  go m (length xs + 1) (last xs)
  where
    go m t prev
      | t > limit = pure prev
      | otherwise = do
        cur <-
          A.readM m prev >>= \case
            0 -> pure 0
            pt -> pure (t - pt - 1)
        A.writeM m prev (t - 1)
        go m (t + 1) cur

-- My solution: Mut HashTable ~27sec
solveForMM :: Int -> [Int] -> Int
solveForMM limit xs = runST $ do
  m <- MHM.fromList $ zip xs [1 ..]
  go m (length xs + 1) (last xs)
  where
    go :: HashTable s Int Int -> Int -> Int -> ST s Int
    go m t prev
      | t > limit = pure prev
      | otherwise = do
        cur <-
          MHM.lookup m prev >>= \case
            Just pt | pt > 0 -> pure (t - pt - 1)
            _ -> pure 0
        MHM.insert m prev (t - 1)
        go m (t + 1) cur
