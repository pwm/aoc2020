module AoC.Days.Day05 where

import AoC.Prelude
import Data.List ((\\))

parse :: String -> Maybe [Seat]
parse = traverse pathToSeat . fmap strToPath . lines

solveA :: [Seat] -> Int
solveA = fromMaybe 0 . maximumOf (folded . #id)

solveB :: [Seat] -> Int
solveB = fromMaybe 0 . firstOf (folded . #id) . findMySeat

rows, cols :: [Int]
rows = [0 .. 127]
cols = [0 .. 7]

findMySeat :: [Seat] -> Maybe Seat
findMySeat seats = liftA2 mkSeat (myRow seats ^? folded . #row) (myCol seats)
  where
    myRow :: [Seat] -> [Seat]
    myRow =
      catMaybes
        . sequenceA
        . firstOf (folded . filtered ((== 7) . length))
        . groupBy (\a b -> a ^. #row == b ^. #row)
        . sortOn (view #row)
    myCol :: [Seat] -> Maybe Int
    myCol ss = head $ cols \\ myRow ss ^.. folded . #col

data Seat = MkSeat
  { row :: Int,
    col :: Int,
    id :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)

mkSeat :: Int -> Int -> Seat
mkSeat r c = MkSeat r c (r * 8 + c)

pathToSeat :: Path -> Maybe Seat
pathToSeat p = liftA2 mkSeat (walk rows (p ^. #isF)) (walk cols (p ^. #isL))
  where
    walk :: [Int] -> [Bool] -> Maybe Int
    walk xs = head . foldl' go xs
      where
        go :: [Int] -> Bool -> [Int]
        go ys b = (if b then take else drop) (length ys `div` 2) ys

data Path = MkPath
  { isF :: [Bool],
    isL :: [Bool]
  }
  deriving stock (Eq, Ord, Show, Generic)

strToPath :: String -> Path
strToPath s = MkPath ((== 'F') <$> take 7 s) ((== 'L') <$> drop 7 s)
