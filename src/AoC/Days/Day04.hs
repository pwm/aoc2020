module AoC.Days.Day04 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Megaparsec.Char.Lexer qualified as L

parse :: String -> Maybe [PassportLike]
parse = parseFileWith passportLikeP

solveA :: [PassportLike] -> Int
solveA =
  length . filter identity . fmap (hasKeys (Set.fromList reqs))
  where
    reqs = [BYR, IYR, EYR, HGT, HCL, ECL, PID]

solveB :: [PassportLike] -> Int
solveB = length . mapMaybe parsePassport

type PassportLike = Map Key String

passportLikeP :: Parser PassportLike
passportLikeP = Map.fromList <$> (some keyValP <* sc)

keyValP :: Parser (Key, String)
keyValP = (,) <$> keyP <* char ':' <*> valP

valP :: Parser String
valP =
  some (digitChar <|> lowerChar <|> char '#')
    <* (spaceChar <|> newline)

data Key
  = BYR -- Birth Year
  | IYR -- Issue Year
  | EYR -- Expiration Year
  | HGT -- Height
  | HCL -- Hair Color
  | ECL -- Eye Color
  | PID -- Passport Id
  | CID -- Country Id
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

keyP :: Parser Key
keyP = enumParser (fmap toLower . show) $ \case
  "byr" -> Just BYR
  "iyr" -> Just IYR
  "eyr" -> Just EYR
  "hgt" -> Just HGT
  "hcl" -> Just HCL
  "ecl" -> Just ECL
  "pid" -> Just PID
  "cid" -> Just CID
  _ -> Nothing

data Passport = UnsafeMkPassport
  { byr :: Int,
    iyr :: Int,
    eyr :: Int,
    hgt :: (Int, Metric),
    hcl :: String,
    ecl :: EyeColour,
    pid :: String
  }
  deriving stock (Eq, Ord, Show, Generic)

parsePassport :: PassportLike -> Maybe Passport
parsePassport pl =
  UnsafeMkPassport
    <$> valForKeyP BYR birthYearP
    <*> valForKeyP IYR issueYearP
    <*> valForKeyP EYR expirationYearP
    <*> valForKeyP HGT heightP
    <*> valForKeyP HCL hairColorP
    <*> valForKeyP ECL eyeColourP
    <*> valForKeyP PID passportIdP
  where
    valForKeyP :: Key -> Parser a -> Maybe a
    valForKeyP k p = pl !? k >>= parseMaybe p

birthYearP :: Parser Int
birthYearP = numOfP $ \x -> 1920 <= x && x <= 2002

issueYearP :: Parser Int
issueYearP = numOfP $ \x -> 2010 <= x && x <= 2020

expirationYearP :: Parser Int
expirationYearP = numOfP $ \x -> 2020 <= x && x <= 2030

heightP :: Parser (Int, Metric)
heightP = do
  x <- L.decimal
  metric <- metricP
  guard $ inRange x metric
  pure (x, metric)
  where
    inRange :: Int -> Metric -> Bool
    inRange x = \case
      Cm -> 150 <= x && x <= 193
      In -> 59 <= x && x <= 76

data Metric = Cm | In
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

metricP :: Parser Metric
metricP = enumParser (fmap toLower . show) $ \case
  "cm" -> Just Cm
  "in" -> Just In
  _ -> Nothing

hairColorP :: Parser String
hairColorP = do
  h <- char '#'
  t <- count 6 (digitChar <|> oneOf ['a' .. 'f'])
  pure (h : t)

data EyeColour = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

eyeColourP :: Parser EyeColour
eyeColourP = enumParser (fmap toLower . show) $ \case
  "amb" -> Just Amb
  "blu" -> Just Blu
  "brn" -> Just Brn
  "gry" -> Just Gry
  "grn" -> Just Grn
  "hzl" -> Just Hzl
  "oth" -> Just Oth
  _ -> Nothing

passportIdP :: Parser String
passportIdP = count 9 digitChar

numOfP :: (Num a) => (a -> Bool) -> Parser a
numOfP p = L.decimal >>= predToP p
