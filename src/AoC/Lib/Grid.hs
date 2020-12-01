module AoC.Lib.Grid where

import AoC.Prelude
import Data.Map.Strict qualified as Map

type Pos = (Int, Int)

type GridOf a = Map Pos a

data Dir = U | R | D | L
  deriving stock (Show, Eq, Ord)

d2p :: Dir -> Pos
d2p = \case
  U -> (-1, 0)
  R -> (0, 1)
  D -> (1, 0)
  L -> (0, -1)

step :: Pos -> Dir -> Pos
step p d = p <+> d2p d

move :: Pos -> [Dir] -> Pos
move = foldr (flip step)

(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

adj4, adj8 :: Pos -> [Pos]
adj4 p = (p <+>) <$> n4
adj8 p = (p <+>) <$> n8

n4, n8 :: [Pos]
n4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]
n8 = filter (/= (0, 0)) (mkSquare (-1) 1)

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
    go :: String -> (Pos, a) -> State Int String
    go m ((x, _), b) = do
      c <- get
      if c == x
        then pure (m <> [draw b])
        else put (c + 1) >> pure (m <> "\n" <> [draw b])

roundTripGrid :: (Char -> Maybe a) -> (a -> Char) -> String -> Bool
roundTripGrid parseCell printCell s =
  (printGrid printCell <$> parseGrid parseCell s) == Just s
