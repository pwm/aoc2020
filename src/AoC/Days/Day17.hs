module AoC.Days.Day17 where

import AoC.Lib.Grid
import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

parse :: String -> Maybe Slice
parse = parseGrid parseCell

solveA :: Slice -> Int
solveA = numActive . view #space . applyTimes 6 iter3D . mkCube3D

solveB :: Slice -> Int
solveB = numActive . view #space . applyTimes 6 iter4D . mkCube4D

numActive :: Map k Bool -> Int
numActive = length . Map.filter (== True)

iter3D :: Cube3D -> Cube3D
iter3D c = growCube3D c & #space %~ (\s3 -> evalState (Map.traverseWithKey go s3) s3)
  where
    go :: (MonadState Space3D m) => P3D -> Bool -> m Bool
    go p _ = nextState adj3D p <$> get

iter4D :: Cube4D -> Cube4D
iter4D c = growCube4D c & #space %~ (\s4 -> evalState (Map.traverseWithKey go s4) s4)
  where
    go :: (MonadState Space4D m) => P4D -> Bool -> m Bool
    go p _ = nextState adj4D p <$> get

-- Util

nextState :: Ord k => (k -> [k]) -> k -> Map k Bool -> Bool
nextState adj p s
  | isActive s p && numNeighbours `elem` [2, 3] = True
  | not (isActive s p) && numNeighbours == 3 = True
  | otherwise = False
  where
    numNeighbours = length $ filter (== True) $ fmap (isActive s) (adj p)

isActive :: Ord k => Map k Bool -> k -> Bool
isActive s p = Just True == (s !? p)

fillSpace :: Ord k => k -> Map k Bool -> Map k Bool
fillSpace = Map.alter $ \case
  Nothing -> Just False
  Just v -> Just v

-- 4D

growCube4D :: Cube4D -> Cube4D
growCube4D c =
  let (lo, hi) = bimap (subtract 1) (+ 1) (c ^. #bounds)
   in c & #bounds .~ (lo, hi)
        & #space .~ foldr fillSpace (c ^. #space) (mkSurface4D (lo, hi))

mkCube4D :: Slice -> Cube4D
mkCube4D s2 =
  MkCube
    { bounds = bounds2D s2,
      space = foldr fillSpace (Map.mapKeys (uncurry (,,0,0)) s2) (mkVolume4D $ bounds2D s2)
    }

type Cube4D = Cube Space4D

type Space4D = Map P4D Bool

type P4D = (Int, Int, Int, Int)

adj4D :: P4D -> [P4D]
adj4D p = (p <<<+>>>) <$> filter (/= (0, 0, 0, 0)) (mkVolume4D (-1, 1))

(<<<+>>>) :: P4D -> P4D -> P4D
(x1, y1, z1, w1) <<<+>>> (x2, y2, z2, w2) = (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

mkVolume4D :: Bounds -> [P4D]
mkVolume4D (l, h) =
  let r = [l .. h]
   in [(x, y, z, w) | x <- r, y <- r, z <- r, w <- r]

mkSurface4D :: Bounds -> [P4D]
mkSurface4D (l, h) =
  let r = [l .. h]
   in [ (x, y, z, w)
        | x <- r,
          y <- r,
          z <- r,
          w <- r,
          minimum [x, y, z, w] == l || maximum [x, y, z, w] == h
      ]

-- 3D

growCube3D :: Cube3D -> Cube3D
growCube3D c =
  let (lo, hi) = bimap (subtract 1) (+ 1) (c ^. #bounds)
   in c & #bounds .~ (lo, hi)
        & #space .~ foldr fillSpace (c ^. #space) (mkSurface3D (lo, hi))

mkCube3D :: Slice -> Cube3D
mkCube3D s2 =
  MkCube
    { bounds = bounds2D s2,
      space = foldr fillSpace (Map.mapKeys (uncurry (,,0)) s2) (mkVolume3D $ bounds2D s2)
    }

type Cube3D = Cube Space3D

type Space3D = Map P3D Bool

type P3D = (Int, Int, Int)

adj3D :: P3D -> [P3D]
adj3D p = (p <<+>>) <$> filter (/= (0, 0, 0)) (mkVolume3D (-1, 1))

(<<+>>) :: P3D -> P3D -> P3D
(x1, y1, z1) <<+>> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

mkVolume3D :: Bounds -> [P3D]
mkVolume3D (l, h) =
  let r = [l .. h]
   in [(x, y, z) | x <- r, y <- r, z <- r]

mkSurface3D :: Bounds -> [P3D]
mkSurface3D (l, h) =
  let r = [l .. h]
   in [ (x, y, z)
        | x <- r,
          y <- r,
          z <- r,
          minimum [x, y, z] == l || maximum [x, y, z] == h
      ]

-- Parsing

data Cube a = MkCube
  { bounds :: Bounds,
    space :: a
  }
  deriving stock (Show, Eq, Ord, Generic)

type Bounds = (Int, Int)

type Slice = GridOf Bool

bounds2D :: Slice -> (Int, Int)
bounds2D s = (Set.findMin (xs <> ys), Set.findMax (xs <> ys))
  where
    xs = Set.map fst $ Map.keysSet s
    ys = Set.map snd $ Map.keysSet s

parseCell :: Char -> Maybe Bool
parseCell = \case
  '.' -> Just False
  '#' -> Just True
  _ -> Nothing

printCell :: Bool -> Char
printCell = \case
  False -> '.'
  True -> '#'
