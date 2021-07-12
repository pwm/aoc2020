module AoC.Days.Day21 where

import AoC.Lib.Parser
import AoC.Prelude hiding (head)
import Control.Monad.Logic
import Data.List (delete)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Prelude (head)

parse :: String -> Maybe [Food]
parse = parseMaybe (lineP `sepEndBy` newline)

solveA :: [Food] -> Int
solveA = length . ingredients . snd . solve

solveB :: [Food] -> String
solveB = intercalate "," . Map.elems . fst . solve

solve :: [Food] -> (AllergenMap, [Food])
solve = observe . go mempty
  where
    go :: AllergenMap -> [Food] -> Logic (AllergenMap, [Food])
    go m foods = do
      guard (invariant m foods)
      if done foods
        then pure (m, foods)
        else do
          (foods', (i, a)) <- pick (candidates foods)
          go (Map.insert a i m) foods'

invariant :: AllergenMap -> [Food] -> Bool
invariant m foods =
  Set.null (knownAllergens `Set.intersection` unknownAllergens)
  where
    knownAllergens = Map.keysSet m
    unknownAllergens = Set.fromList (allergens foods)

done :: [Food] -> Bool
done = null . allergens

pick :: [a] -> LogicT m a
pick = asum . fmap pure

candidates :: [Food] -> [([Food], (Ingredient, Allergen))]
candidates foods = removeChoice foods <$> choicesForNextA foods
  where
    choicesForNextA :: [Food] -> [(Ingredient, Allergen)]
    choicesForNextA = uncurry zip . second (repeat . head) . head . filter (not . null . snd)
    removeChoice :: [Food] -> (Ingredient, Allergen) -> ([Food], (Ingredient, Allergen))
    removeChoice fs (i, a) = (dels <$> fs, (i, a))
      where
        dels :: Food -> Food
        dels (is, as)
          | i `elem` is && a `elem` as = (delete i is, delete a as)
          | i `elem` is = (delete i is, as)
          | otherwise = (is, as)

ingredients :: [Food] -> [Ingredient]
ingredients = concatMap fst

allergens :: [Food] -> [Allergen]
allergens = concatMap snd

type Ingredient = String

type Allergen = String

type Food = ([Ingredient], [Allergen])

type AllergenMap = Map Allergen Ingredient

lineP :: Parser Food
lineP = do
  is <- some lowerChar `sepEndBy` space1 <* string "(contains "
  as <- some lowerChar `sepBy` lexeme "," <* char ')'
  pure (is, as)
