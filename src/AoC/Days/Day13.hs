module AoC.Days.Day13 where

import AoC.Lib.SimpleParser
import AoC.Prelude hiding (head)
import Data.Bitraversable (bisequenceA)
import Data.List.Split (splitOn)
import Math.NumberTheory.Moduli.Chinese
import Prelude (head)

parse :: String -> Maybe (Int, [Int])
parse s = bisequenceA (stringToInt (head ls), traverse stringToInt ss)
  where
    ls = lines s
    ss = fmap (\c -> if c == "x" then "0" else c) $ splitOn "," $ head $ drop 1 ls

solveA :: (Int, [Int]) -> Int
solveA (n, xs) = (closestDep - n) * id
  where
    (closestDep, id) = findClosest n (filter (/= 0) xs)

solveB :: (Int, [Int]) -> Int
solveB =
  fromMaybe 0
    . fmap fromIntegral
    . chineseRemainder
    . fmap (bimap fromIntegral fromIntegral)
    . fmap (\(a, b) -> ((b - a) `mod` b, b))
    . filter ((/= 0) . snd)
    . zip [0 ..]
    . snd

findClosest :: Int -> [Int] -> (Int, Int)
findClosest n xs =
  minimumBy (\(x, _) (y, _) -> x `compare` y) $
    zip (zipWith (*) ((+ 1) . (n `div`) <$> xs) xs) xs
