module AoC.Days.Day14 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [(Mask, [Op])]
parse = parseFileWith blockP

solveA :: [(Mask, [Op])] -> Word64
solveA = sum . Map.elems . execComp comp1

solveB :: [(Mask, [Op])] -> Word64
solveB = sum . Map.elems . execComp comp2

execComp :: (Mask -> [Op] -> [Op]) -> [(Mask, [Op])] -> Map Word64 Word64
execComp comp = flip execState mempty . traverse go
  where
    go = modify . Map.union . Map.fromList . uncurry comp

comp1 :: Mask -> [Op] -> [Op]
comp1 m = fmap (second (mask1 m))

mask1 :: Mask -> Word64 -> Word64
mask1 = flip (foldr go)
  where
    go :: (Int, MaskType) -> Word64 -> Word64
    go (x, MT0) w = w `clearBit` x
    go (x, MT1) w = w `setBit` x
    go (_, MTX) w = w

comp2 :: Mask -> [Op] -> [Op]
comp2 m = concatMap ((\(ws, w) -> zip ws (repeat w)) . first (mask2 m))

mask2 :: Mask -> Word64 -> [Word64]
mask2 m w = foldr go [w] m
  where
    go :: (Int, MaskType) -> [Word64] -> [Word64]
    go (_, MT0) ws = ws
    go (x, MT1) ws = (`setBit` x) <$> ws
    go (x, MTX) ws = ((`clearBit` x) <$> ws) <> ((`setBit` x) <$> ws)

type Op = (Word64, Word64)

type Mask = [(Int, MaskType)]

data MaskType = MT0 | MT1 | MTX
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

blockP :: Parser (Mask, [Op])
blockP = (,) <$> maskP <*> some opP

maskP :: Parser Mask
maskP = do
  _ <- string "mask = "
  s <- count 36 (choice [char '1', char '0', char 'X']) <* sc
  maybeToP (fmap (zip [0 ..]) . traverse parseMmaskType . reverse) s
  where
    parseMmaskType :: Char -> Maybe MaskType
    parseMmaskType = \case
      '0' -> Just MT0
      '1' -> Just MT1
      'X' -> Just MTX
      _ -> Nothing

opP :: Parser Op
opP = (,) <$> (string "mem" *> squareBracketsP w64P) <*> (lexeme "=" *> w64P)
