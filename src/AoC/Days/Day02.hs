module AoC.Days.Day02 where

import AoC.Lib.Parser
import AoC.Prelude
import Text.Megaparsec.Char.Lexer qualified as L

parse :: String -> Maybe [Row]
parse = parseFileWith passP

solveA :: [Row] -> Int
solveA = length . filter isValidRowA

solveB :: [Row] -> Int
solveB = length . filter isValidRowB

isValidRowA :: Row -> Bool
isValidRowA r = r ^. #x <= i && i <= (r ^. #y)
  where
    i = length $ filter (== r ^. #c) (r ^. #pw)

isValidRowB :: Row -> Bool
isValidRowB r = isLetterAt (r ^. #x) `xor` isLetterAt (r ^. #y)
  where
    isLetterAt :: Int -> Bool
    isLetterAt i = charAt (i - 1) (r ^. #pw) == Just (r ^. #c)

data Row = MkRow
  { x :: Int,
    y :: Int,
    c :: Char,
    pw :: String
  }
  deriving stock (Eq, Ord, Show, Generic)

passP :: Parser Row
passP =
  MkRow
    <$> (L.decimal <* char '-')
    <*> (L.decimal <* sc)
    <*> (lowerChar <* symbol ":")
    <*> (some lowerChar <* sc)
