module AoC.Days.Day20 where

import AoC.Lib.Grid
import AoC.Lib.Parser
import AoC.Prelude hiding (head)
import Control.Monad.Logic
import Data.List ((\\))
import Data.List.Split (chunksOf)
import Data.Map.Strict ((!), (!?))
import Data.Map.Strict qualified as Map
import Prelude (head, last)

parse :: String -> Maybe Env
parse = fmap toEnv . parseMaybe tilesP
  where
    toEnv :: [(Int, Tile)] -> Env
    toEnv ps =
      MkEnv
        { tiles = snd <$> ps,
          hashIdMap = Map.fromList $ first hash . swap <$> ps,
          tileGrid = mempty,
          cellGrid = mempty
        }

solveA :: Env -> Int
solveA = product . corners . idSquare . assemble
  where
    idSquare :: Env -> [[Int]]
    idSquare env =
      toSquareList
        . toList
        . fmap (hashToId env)
        . view #tileGrid
        $ env
    corners :: [[Int]] -> [Int]
    corners idS =
      let fs = [head . head, head . last, last . head, last . last]
       in fmap (\f -> f idS) fs

solveB :: Env -> Int
solveB env = numHash image - monsters * length monster
  where
    image = view #cellGrid . mapify . assemble $ env
    monsters =
      head
        . filter (> 0)
        . fmap (numMonsters . listToGrid)
        . allOrient
        . toSquareList
        . toList
        $ image

data Env = MkEnv
  { tiles :: [Tile],
    -- lookup the id of a tile by its stuctural hash via hashToId
    hashIdMap :: Map Int Int,
    tileGrid :: GridOf Tile,
    cellGrid :: GridOf Int
  }
  deriving stock (Show, Eq, Ord, Generic)

type Tile = [[Int]]

mapify :: Env -> Env
mapify env = env & #cellGrid .~ mapify' (env ^. #tileGrid)
  where
    mapify' :: GridOf Tile -> GridOf Int
    mapify' =
      listToGrid
        . toSquareList
        . concat
        . concat
        . concatMap transpose
        . toSquareList
        . fmap crop
        . toList

crop :: Tile -> Tile
crop = fmap (dropEnd 1 . drop 1) . dropEnd 1 . drop 1

numHash :: GridOf Int -> Int
numHash = Map.size . Map.filter (== 1)

numMonsters :: GridOf Int -> Int
numMonsters g =
  length
    . filter (== True)
    . fmap (hasMonster g . mkMonster)
    . Map.keys
    $ g

mkMonster :: Pos -> [Pos]
mkMonster pos = fmap (pos <+>) monster

monster :: [Pos]
monster =
  mconcat
    [ zip (repeat 0) [18],
      zip (repeat 1) [0, 5, 6, 11, 12, 17, 18, 19],
      zip (repeat 2) [1, 4, 7, 10, 13, 16]
    ]

hasMonster :: GridOf Int -> [Pos] -> Bool
hasMonster g ps = length (monsterBits ps) == length ps
  where
    monsterBits :: [Pos] -> [Int]
    monsterBits = filter (== 1) . mapMaybe (g !?)

hashToId :: Env -> Tile -> Int
hashToId env =
  headOr 0
    . mapMaybe (\t -> view #hashIdMap env !? hash t)
    . allOrient

assemble :: Env -> Env
assemble = observe . go
  where
    go :: Env -> Logic Env
    go env
      | done env = pure env
      | otherwise = do
        nextTile <- choose $ candidates env
        go $ env & #tileGrid %~ Map.insert (nextPos env) nextTile

done :: Env -> Bool
done env = length (env ^. #tiles) == length (env ^. #tileGrid)

candidates :: Env -> [Tile]
candidates env = case lastPos env of
  Nothing -> allOrients (env ^. #tiles)
  Just lastP -> filter canConnect freeTiles
    where
      freeTiles = allOrients (env ^. #tiles) \\ allOrients (toList (env ^. #tileGrid))
      canConnect :: Tile -> Bool
      canConnect t =
        let (nextP, dir) = nextPosWithDir env
         in if dir == D
              then down ((env ^. #tileGrid) ! step4 nextP U) == up t
              else right ((env ^. #tileGrid) ! lastP) == left t

lastPos :: Env -> Maybe Pos
lastPos = fmap fst . Map.lookupMax . view #tileGrid

nextPos :: Env -> Pos
nextPos = fst . nextPosWithDir

nextPosWithDir :: Env -> (Pos, Dir4)
nextPosWithDir env = case lastPos env of
  Nothing -> ((0, 0), D)
  Just (x, y)
    | y + 1 < squareSize (env ^. #tiles) -> ((x, y + 1), R)
    | otherwise -> ((x + 1, 0), D)

allOrients :: [[[a]]] -> [[[a]]]
allOrients = concatMap allOrient

allOrient :: [[a]] -> [[[a]]]
allOrient t = fmap (\f -> f t) (rot90s <> ((. reverse) <$> rot90s))
  where
    rot90s = [rot90 0, rot90 1, rot90 2, rot90 3]

rot90 :: Int -> [[a]] -> [[a]]
rot90 n = applyTimes n (fmap reverse . transpose)

up, down, left, right :: [[a]] -> [a]
up = head
down = last
left = fmap head
right = fmap last

toSquareList :: [a] -> [[a]]
toSquareList l = chunksOf (squareSize l) l

squareSize :: [a] -> Int
squareSize = sqrtInt . length

tilesP :: Parser [(Int, Tile)]
tilesP = tileP `sepEndBy` newline

tileP :: Parser (Int, Tile)
tileP = do
  id <- lexeme "Tile" *> intP <* lexeme ":"
  xs <- some (char '.' <|> char '#') `sepEndBy` newline
  bs <- traverse (traverse cellP) xs
  pure (id, bs)

cellP :: Char -> Parser Int
cellP = maybeToP $ \case
  '.' -> Just 0
  '#' -> Just 1
  _ -> Nothing
