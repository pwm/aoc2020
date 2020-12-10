module AoC.Days.Day10 where

import AoC.Lib.SimpleParser
import AoC.Prelude hiding (head)
import Data.List (span)
import Prelude (head, (!!))

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = uncurry (*) . bimap length length . span (< 3) . diffs . sort . ends
  where
    diffs :: [Int] -> [Int]
    diffs xs = sort $ zipWith (-) (drop 1 xs) xs

solveB :: [Int] -> Int
solveB xs =
  let ys = fmap (!! 1) . touching3s . sort . ends $ xs
      a = length ys
      b = length . touching3s $ ys
   in 2 ^ a - sum (excludes a b)

ends :: [Int] -> [Int]
ends xs = 0 : (maximum xs + 3) : xs

touching3s :: [Int] -> [[Int]]
touching3s = filter (\ys -> (ys !! 2) - head ys == 2) . slicesOf 3

excludes :: Int -> Int -> [Int]
excludes n x = snd $ foldr go (2 ^ (n - 3), []) [1 .. x]
  where
    go :: Int -> (Int, [Int]) -> (Int, [Int])
    go _ (s, ss) = (s - (s `div` 8), s : ss)
