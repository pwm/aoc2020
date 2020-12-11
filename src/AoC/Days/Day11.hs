module AoC.Days.Day11 where

import AoC.Lib.Grid
import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map

parse :: String -> Maybe Grid
parse = parseGrid parseCell

solveA :: Grid -> Int
solveA = numTaken . fixpoint (iter ruleA)

solveB :: Grid -> Int
solveB = numTaken . fixpoint (iter ruleB)

numTaken :: Grid -> Int
numTaken = length . Map.filter (== Taken)

iter :: (Grid -> Pos -> Cell -> Cell) -> Grid -> Grid
iter rule g = Map.mapWithKey (rule g) g

ruleA :: Grid -> Pos -> Cell -> Cell
ruleA g p c
  | c == Vacant && notElem Taken adjCells = Taken
  | c == Taken && length (filter (== Taken) adjCells) >= 4 = Vacant
  | otherwise = c
  where
    adjCells = lookups g (adj8 p)

ruleB :: Grid -> Pos -> Cell -> Cell
ruleB g p c
  | c == Vacant && notElem Taken closestSeats = Taken
  | c == Taken && length (filter (== Taken) closestSeats) >= 5 = Vacant
  | otherwise = c
  where
    closestSeats = mapMaybe (closestSeat g p) (enumerate @Dir8)

closestSeat :: Grid -> Pos -> Dir8 -> Maybe Cell
closestSeat g p d = case g !? step8 p d of
  Nothing -> Nothing
  Just c | c /= Floor -> Just c
  _ -> closestSeat g (step8 p d) d

type Grid = GridOf Cell

data Cell
  = Taken
  | Vacant
  | Floor
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

parseCell :: Char -> Maybe Cell
parseCell = \case
  '#' -> Just Taken
  'L' -> Just Vacant
  '.' -> Just Floor
  _ -> Nothing

printCell :: Cell -> Char
printCell = \case
  Taken -> '#'
  Vacant -> 'L'
  Floor -> '.'
