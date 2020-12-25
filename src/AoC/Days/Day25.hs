module AoC.Days.Day25 where

import AoC.Lib.SimpleParser
import AoC.Prelude hiding (head)
import Data.IntMap ((!))
import Data.IntMap qualified as IM
import Prelude (head)

parse :: String -> Maybe (Int, Int)
parse = l2p <=< stringToInts

solveA :: (Int, Int) -> Int
solveA (p, q) = let m = 20_201_227 in expMod p (discreteLog m q 7) m

solveB :: (Int, Int) -> ()
solveB _ = ()

discreteLog :: Int -> Int -> Int -> Int
discreteLog m x base = p - (xs ! k)
  where
    s = ceiling (sqrt @Double (fromIntegral m))
    xs = IM.fromList [(((x `mod` m) * expMod base r m) `mod` m, r) | r <- [0 .. s]]
    ys = IM.fromList [(expMod base ((s -1) * r) m, (s -1) * r) | r <- [1 .. s]]
    (k, p) = head $ IM.toList $ IM.filterWithKey (\k_ _ -> IM.member k_ xs) ys

expMod :: Int -> Int -> Int -> Int
expMod a b c
  | a == 0 = 0
  | b == 0 = 1
  | b `mod` 2 == 0 = let y = expMod a (b `div` 2) c in (((y * y) `mod` c) + c) `mod` c
  | otherwise = let y = ((a `mod` c) * expMod a (b -1) c `mod` c) `mod` c in (y + c) `mod` c

