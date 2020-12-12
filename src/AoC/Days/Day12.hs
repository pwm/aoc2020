module AoC.Days.Day12 where

import AoC.Lib.Grid hiding (Dir4 (..), Dir8 (..))
import AoC.Lib.Parser
import AoC.Prelude

parse :: String -> Maybe [Move]
parse = parseFileWith moveP

solveA :: [Move] -> Int
solveA = manhattan (0, 0) . view #pos . sail plain

solveB :: [Move] -> Int
solveB = manhattan (0, 0) . view #pos . sail waypoint

sail :: (Ship -> Move -> Ship) -> [Move] -> Ship
sail f = foldl' f (MkShip (0, 0) FE (-1, 10))

plain :: Ship -> Move -> Ship
plain s = \case
  (N, n) -> s & #pos %~ steps n stepN
  (E, n) -> s & #pos %~ steps n stepE
  (S, n) -> s & #pos %~ steps n stepS
  (W, n) -> s & #pos %~ steps n stepW
  (L, n) -> s & #face %~ applyTimes (n `div` 90) turnsL
  (R, n) -> s & #face %~ applyTimes (n `div` 90) turnsR
  (F, n) -> s & #pos %~ steps n (faceToPos (s ^. #face))

waypoint :: Ship -> Move -> Ship
waypoint s = \case
  (N, n) -> s & #wp %~ steps n stepN
  (E, n) -> s & #wp %~ steps n stepE
  (S, n) -> s & #wp %~ steps n stepS
  (W, n) -> s & #wp %~ steps n stepW
  (L, n) -> s & #wp %~ applyTimes (n `div` 90) rotatesL
  (R, n) -> s & #wp %~ applyTimes (n `div` 90) rotatesR
  (F, n) -> s & #pos %~ steps n (s ^. #wp)

steps :: Int -> Pos -> Pos -> Pos
steps n p = flip move (replicate n p)

faceToPos :: Face -> Pos
faceToPos = \case
  FN -> stepN
  FE -> stepE
  FS -> stepS
  FW -> stepW

rotatesL, rotatesR :: Pos -> Pos
rotatesL (x, y) = (- y, x)
rotatesR (x, y) = (y, - x)

turnsL, turnsR :: Face -> Face
turnsL = \case
  FN -> FW
  FW -> FS
  FS -> FE
  FE -> FN
turnsR = \case
  FN -> FE
  FE -> FS
  FS -> FW
  FW -> FN

data Ship = MkShip
  { pos :: Pos,
    face :: Face,
    wp :: Pos
  }
  deriving stock (Show, Generic)

data Face = FN | FE | FS | FW
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

type Move = (MoveType, Int)

data MoveType = N | E | S | W | L | R | F
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

moveP :: Parser Move
moveP = (,) <$> moveTypeP <*> intP
  where
    moveTypeP :: Parser MoveType
    moveTypeP = enumParser show $ \case
      "N" -> Just N
      "E" -> Just E
      "S" -> Just S
      "W" -> Just W
      "L" -> Just L
      "R" -> Just R
      "F" -> Just F
      _ -> Nothing
