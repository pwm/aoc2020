module AoC.Days.Day10 where

import AoC.Lib.Memo
import AoC.Lib.SimpleParser
import AoC.Prelude hiding (head)
import Data.List (span)

parse :: String -> Maybe [Int]
parse = stringToInts

solveA :: [Int] -> Int
solveA = uncurry (*) . bimap length length . span (< 3) . diffs . sort . ends
  where
    diffs :: [Int] -> [Int]
    diffs xs = sort $ zipWith (-) (drop 1 xs) xs

solveB :: [Int] -> Int
solveB xs = memoMap (process ys) (maximum ys)
  where
    ys = sort (ends xs)

process :: (Monad m) => [Int] -> (Int -> m Int) -> Int -> m Int
process xs rec n
  | n == 1 || n == 0 = pure 1
  | n `notElem` xs = pure 0
  | otherwise = do
    a <- if n - 1 `elem` xs then rec (n - 1) else pure 0
    b <- if n - 2 `elem` xs then rec (n - 2) else pure 0
    c <- if n - 3 `elem` xs then rec (n - 3) else pure 0
    pure (a + b + c)

ends :: [Int] -> [Int]
ends xs = 0 : (maximum xs + 3) : xs
