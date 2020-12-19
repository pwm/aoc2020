module AoC.Days.Day19 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> Maybe (Rules, [String])
parse = parseMaybe $ do
  rules <- Map.fromList <$> some (ruleLineP <* sc)
  inputs <- some (some (char 'a' <|> char 'b') <* sc) <* sc
  pure (rules, inputs)

solveA :: (Rules, [String]) -> Int
solveA (rs, ws) =
  length $ filter (not . null) $ parseMaybe (buildWithMega rs 0) <$> ws

solveB :: (Rules, [String]) -> Int
solveB (rs, ws) =
  length $ filter (not . null) $ P.readP_to_S (buildWithReadP (amendRules rs) 0 *> P.eof) <$> ws

buildWithMega :: Rules -> Int -> Parser String
buildWithMega m i = case m ! i of
  Lit c -> string c
  Seq xs -> go xs
  Par xs ys -> asum $ fmap try [go xs, go ys]
  where
    go = fmap mconcat . traverse (buildWithMega m)

-- We use ReadP as it uses [(a, String)] and thus can represent multiple results
-- as opposed to Maybe (a, String) which can only represent zero or one.
-- This is important as with open ended expansion default greedy algos don't
-- know where to stop and thus would consider the pattern invalid.
buildWithReadP :: Rules -> Int -> ReadP String
buildWithReadP m i = case m ! i of
  Lit c -> P.string c
  Seq xs -> go xs
  Par xs ys -> asum [go xs, go ys]
  where
    go = fmap mconcat . traverse (buildWithReadP m)

amendRules :: Rules -> Rules
amendRules = Map.mapWithKey (curry go)
  where
    go :: (Int, Rule) -> Rule
    go = \case
      (8, Seq [42]) -> Par [42] [42, 8]
      (11, Seq [42, 31]) -> Par [42, 31] [42, 11, 31]
      (_, r) -> r

type Rules = Map Int Rule

data Rule
  = Lit String
  | Seq [Int]
  | Par [Int] [Int]
  deriving stock (Show, Eq, Ord, Generic)

ruleLineP :: Parser (Int, Rule)
ruleLineP = do
  n <- intP <* lexeme ":"
  rule <- ruleP =<< (try (pure . pure <$> litP) <|> refP)
  pure (n, rule)

litP :: Parser String
litP = string "\"" *> (string "a" <|> string "b") <* string "\""

refP :: Parser [[String]]
refP = (some digitChar `sepEndBy` char ' ') `sepBy` lexeme "|"

ruleP :: [[String]] -> Parser Rule
ruleP = maybeToP $ \case
  ((x : _) : _) | x `elem` ["a", "b"] -> Just $ Lit x
  xs
    | Just ys <- traverse (traverse (parseMaybe intP)) xs ->
      if
          | [a] <- ys -> Just $ Seq a
          | [a, b] <- ys -> Just $ Par a b
          | otherwise -> Nothing
  _ -> Nothing
