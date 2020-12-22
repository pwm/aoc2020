module AoC.Lib.Grid where

import AoC.Prelude
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

type GridOf a = Map Pos a

--

data Dir4 = U | R | D | L
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

d2p4 :: Dir4 -> Pos
d2p4 d = Map.fromList (zip enumerate n4) ! d

step4 :: Pos -> Dir4 -> Pos
step4 = stepWith d2p4

move4 :: Pos -> [Dir4] -> Pos
move4 = moveWith d2p4

--

data Dir8 = N | NE | E | SE | S | SW | W | NW
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

d2p8 :: Dir8 -> Pos
d2p8 d = Map.fromList (zip enumerate n8) ! d

step8 :: Pos -> Dir8 -> Pos
step8 = stepWith d2p8

move8 :: Pos -> [Dir8] -> Pos
move8 = moveWith d2p8

--

type Pos = (Int, Int)

stepU, stepR, stepD, stepL :: Pos
stepU = (-1, 0)
stepR = (0, 1)
stepD = (1, 0)
stepL = (0, -1)

stepN, stepNE, stepE, stepSE, stepS, stepSW, stepW, stepNW :: Pos
stepN = stepU
stepNE = (-1, 1)
stepE = stepR
stepSE = (1, 1)
stepS = stepD
stepSW = (1, -1)
stepW = stepL
stepNW = (-1, -1)

n4, n8 :: [Pos]
n4 = [stepU, stepR, stepD, stepL]
n8 = [stepN, stepNE, stepE, stepSE, stepS, stepSW, stepW, stepNW]

(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

adj4, adj8 :: Pos -> [Pos]
adj4 p = (p <+>) <$> n4
adj8 p = (p <+>) <$> n8

step :: Pos -> Pos -> Pos
step = stepWith identity

stepWith :: (a -> Pos) -> Pos -> a -> Pos
stepWith d2p p d = p <+> d2p d

move :: Pos -> [Pos] -> Pos
move = moveWith identity

moveWith :: (a -> Pos) -> Pos -> [a] -> Pos
moveWith d2p = foldr (flip (stepWith d2p))

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

mkRect :: Int -> Int -> Int -> Int -> [Pos]
mkRect ln hn lm hm = (,) <$> [ln .. hn] <*> [lm .. hm]

mkSquare :: Int -> Int -> [Pos]
mkSquare ln hn = mkRect ln hn ln hn

--

listToGrid :: [[a]] -> GridOf a
listToGrid =
  Map.fromList
    . concatMap (\(x, l) -> fmap (first (x,)) l)
    . zip [0 ..]
    . fmap (zip [0 ..])

parseGrid ::
  forall a.
  (Char -> Maybe a) ->
  String ->
  Maybe (GridOf a)
parseGrid parseCell =
  fmap listToGrid . traverse (traverse parseCell) . lines

printGrid :: forall a. (a -> Char) -> GridOf a -> String
printGrid draw =
  flip evalState 0 . foldM go "" . Map.toAscList
  where
    go :: (MonadState Int m) => String -> (Pos, a) -> m String
    go m ((x, _), b) = do
      c <- get
      if c == x
        then pure (m <> [draw b])
        else put (c + 1) >> pure (m <> "\n" <> [draw b])

roundTripGrid :: (Char -> Maybe a) -> (a -> Char) -> String -> Bool
roundTripGrid parseCell printCell s =
  (printGrid printCell <$> parseGrid parseCell s) == Just s
