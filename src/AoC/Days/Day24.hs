module AoC.Days.Day24 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe [Path]
parse = parseMaybe pathsP

solveA :: [Path] -> Int
solveA = length . init

solveB :: [Path] -> Int
solveB = length . applyTimes 100 iter . init

init :: [Path] -> HexSet
init = flip execState mempty . traverse_ (modify . go)
  where
    go :: Path -> HexSet -> HexSet
    go path s = if Set.member p s then Set.delete p s else Set.insert p s
      where
        p = moveHex (Pos 0 0 0) path

iter :: HexSet -> HexSet
iter s = foldr rule mempty (Set.foldr (\p ps -> adjHex p <> ps) [] s)
  where
    rule :: Pos -> HexSet -> HexSet
    rule p s'
      | isBlack p && (nBlacks p == 0 || nBlacks p > 2) = Set.delete p s'
      | not (isBlack p) && nBlacks p == 2 = Set.insert p s'
      | isBlack p = Set.insert p s'
      | otherwise = s'
      where
        isBlack = (`Set.member` s)
        nBlacks = length . filter (== True) . fmap isBlack . adjHex

type HexSet = Set Pos

type Path = [Dir]

data Pos = Pos Int Int Int
  deriving stock (Show, Eq, Ord, Generic)

data Dir = E | SE | SW | W | NW | NE
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

moveHex :: Pos -> Path -> Pos
moveHex = moveWith d2pHex

moveWith :: (a -> Pos) -> Pos -> [a] -> Pos
moveWith d2p = foldr (flip (stepWith d2p))

stepWith :: (a -> Pos) -> Pos -> a -> Pos
stepWith d2p p d = p <+> d2p d

d2pHex :: Dir -> Pos
d2pHex d = Map.fromList (zip enumerate neighboursHex) ! d

adjHex :: Pos -> [Pos]
adjHex p = (p <+>) <$> neighboursHex

(<+>) :: Pos -> Pos -> Pos
Pos x1 y1 z1 <+> Pos x2 y2 z2 = Pos (x1 + x2) (y1 + y2) (z1 + z2)

neighboursHex :: [Pos]
neighboursHex = [stepE, stepSE, stepSW, stepW, stepNW, stepNE]

stepE, stepSE, stepSW, stepW, stepNW, stepNE :: Pos
stepE = Pos 1 (-1) 0
stepSE = Pos 0 (-1) 1
stepSW = Pos (-1) 0 1
stepW = Pos (-1) 1 0
stepNW = Pos 0 1 (-1)
stepNE = Pos 1 0 (-1)

pathsP :: Parser [Path]
pathsP = some dirP `sepEndBy` newline

dirP :: Parser Dir
dirP = enumParser (fmap toLower . show) $ \case
  "e" -> Just E
  "se" -> Just SE
  "sw" -> Just SW
  "w" -> Just W
  "nw" -> Just NW
  "ne" -> Just NE
  _ -> Nothing
