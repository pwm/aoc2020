module AoC.Lib.Grid where

import AoC.Prelude
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

type Pos = (Int, Int)

type GridOf a = Map Pos a

data Dir4 = U | R | D | L
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

data Dir8 = N | NE | E | SE | S | SW | W | NW
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

n4, n8 :: [Pos]
n4 = [(-1, 0), (0, 1), (1, 0), (0, -1)]
n8 = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

d2p4 :: Dir4 -> Pos
d2p4 d = Map.fromList (zip enumerate n4) ! d

step4 :: Pos -> Dir4 -> Pos
step4 = step d2p4

move4 :: Pos -> [Dir4] -> Pos
move4 = move d2p4

d2p8 :: Dir8 -> Pos
d2p8 d = Map.fromList (zip enumerate n8) ! d

step8 :: Pos -> Dir8 -> Pos
step8 = step d2p8

move8 :: Pos -> [Dir8] -> Pos
move8 = move d2p8

step :: (a -> Pos) -> Pos -> a -> Pos
step d2p p d = p <+> d2p d

move :: (a -> Pos) -> Pos -> [a] -> Pos
move d2p = foldr (flip (step d2p))

(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

adj4, adj8 :: Pos -> [Pos]
adj4 p = (p <+>) <$> n4
adj8 p = (p <+>) <$> n8

mkRect :: Int -> Int -> Int -> Int -> [Pos]
mkRect ln hn lm hm = (,) <$> [ln .. hn] <*> [lm .. hm]

mkSquare :: Int -> Int -> [Pos]
mkSquare ln hn = mkRect ln hn ln hn

parseGrid ::
  forall a.
  (Char -> Maybe a) ->
  String ->
  Maybe (GridOf a)
parseGrid parseCell =
  fmap l2grid . traverse (traverse parseCell) . lines
  where
    l2grid :: [[a]] -> GridOf a
    l2grid =
      Map.fromList
        . concatMap (\(x, l) -> fmap (\(y, v) -> ((x, y), v)) l)
        . zip [0 ..]
        . fmap (zip [0 ..])

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
