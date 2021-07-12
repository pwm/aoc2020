module AoC.Days.Day07 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Graph
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tree

parse :: String -> Maybe AdjMap
parse = fmap Map.fromList . parseFileWith adjListP

solveA :: AdjMap -> Int
solveA = subtract 1 . length . snd . ancestorsOf "shiny gold"

solveB :: AdjMap -> Int
solveB = subtract 1 . uncurry numBagsOf . descendantsOf "shiny gold"

type Key = String

type Vals = [(Int, Key)]

type AdjMap = Map Key Vals

ancestorsOf, descendantsOf :: Key -> AdjMap -> (Key, AdjMap)
ancestorsOf k = (k,) . fst . splitAdjMapOn k
descendantsOf k = (k,) . snd . splitAdjMapOn k

splitAdjMapOn :: Key -> AdjMap -> (AdjMap, AdjMap)
splitAdjMapOn k m =
  let (g, v2d, k2v) = graphFromEdges . fmap (\(x, xs) -> (xs, x, snd <$> xs)) . Map.toList $ m
      filterVerts = Map.restrictKeys m . Set.fromList . fmap (view _2 . v2d)
      as = filterVerts . reachable (transposeG g) <$> k2v k
      ds = filterVerts . reachable g <$> k2v k
   in (fromMaybe mempty as, fromMaybe mempty ds)
 
numBagsOf :: Key -> AdjMap -> Int
numBagsOf k0 m =
  foldTree (\(x, _) xs -> x + x * sum xs)
    . unfoldTree (\(x, k) -> ((x, k), fromMaybe [] (m !? k)))
    $ (1, k0)

adjListP :: Parser (Key, Vals)
adjListP = do
  k <- unwords <$> (some lowerChar <* sc) `manyTill` symbol "bags contain"
  vs <- try someItemP <|> noItemP
  _ <- sc
  pure (k, vs)
  where
    someItemP :: Parser Vals
    someItemP = some $ do
      x <- intP
      k <- unwords <$> (some lowerChar <* sc) `manyTill` (symbol "bags" <|> symbol "bag")
      _ <- (char ',' <* sc) <|> char '.'
      pure (x, k)
    noItemP :: Parser Vals
    noItemP = string "no other bags." >> pure []
