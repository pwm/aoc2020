module AoC.Days.Day16 where

import AoC.Lib.Parser
import AoC.Prelude hiding (head)
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Prelude (head)

parse :: String -> Maybe Doc
parse = parseMaybe docP

solveA :: Doc -> Int
solveA doc =
  sum
    . filter (`notElem` concatMap Set.toList (doc ^. #rules))
    . concat
    $ doc ^. #nearbyTickets

solveB :: Doc -> Int
solveB doc =
  product
    . fmap fst
    . filter ((== "departure") . substring 0 9 . snd . snd)
    . zip (doc ^. #myTicket)
    . Map.toList
    . solveFields
    . potentialFieldsForRows
    . filterInvalids
    $ doc

filterInvalids :: Doc -> Doc
filterInvalids doc = doc & #nearbyTickets %~ filter hasInvalid
  where
    hasInvalid = not . any (`notElem` concatMap Set.toList (doc ^. #rules))

solveFields :: Map Int (Set String) -> Map Int String
solveFields = fmap (head . Set.toList) . fixpoint elim
  where
    elim :: Map Int (Set String) -> Map Int (Set String)
    elim m = fmap (\s -> if Set.size s == 1 then s else s \\ sings m) m
      where
        sings = fold . Map.elems . Map.filter ((== 1) . Set.size)

potentialFieldsForRows :: Doc -> Map Int (Set String)
potentialFieldsForRows doc =
  Map.fromList
    . fmap (second (potentialFieldsForRow (doc ^. #rules)))
    . zip [1 ..]
    . transpose
    $ (doc ^. #nearbyTickets)
  where
    potentialFieldsForRow :: Rules -> [Int] -> Set String
    potentialFieldsForRow rules xs =
      Map.keysSet $ Map.filter (Set.isSubsetOf (Set.fromList xs)) rules

type Rules = Map String (Set Int)

data Doc = MkDoc
  { rules :: Rules,
    myTicket :: [Int],
    nearbyTickets :: [[Int]]
  }
  deriving stock (Show, Eq, Ord, Generic)

docP :: Parser Doc
docP =
  MkDoc
    <$> (rulesP <* sc)
    <*> (myTicketP <* sc)
    <*> (nearbyTicketsP <* sc)

rulesP :: Parser Rules
rulesP = Map.fromList <$> ruleLineP `endBy` newline
  where
    ruleLineP :: Parser (String, Set Int)
    ruleLineP = do
      field <- someTill (lowerChar <|> char ' ') (char ':') <* sc
      minMaxes <- minMaxP `sepBy` string " or "
      pure (field, Set.fromList $ concatMap (\(x, y) -> [x .. y]) minMaxes)
    minMaxP :: Parser (Int, Int)
    minMaxP = (,) <$> (intP <* char '-') <*> Lexer.decimal

myTicketP :: Parser [Int]
myTicketP = (string "your ticket:" <* sc) *> ticketLineP

nearbyTicketsP :: Parser [[Int]]
nearbyTicketsP = (string "nearby tickets:" <* sc) *> ticketLineP `endBy1` newline

ticketLineP :: Parser [Int]
ticketLineP = Lexer.decimal `sepBy1` char ','
