module AoC.Lib.Parser
  ( module X,
    Parser,
    parseFileWith,
    blocksP,
    intP,
    signedIntP,
    lexeme,
    symbol,
    sc,
    enumParser,
    maybeToP,
    predToP,
  )
where

import AoC.Lib.SimpleParser as X
import AoC.Prelude
import Text.Megaparsec as X hiding (State (..))
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void String

parseFileWith :: Parser a -> String -> Maybe [a]
parseFileWith = parseMaybe . blocksP

blocksP :: Parser a -> Parser [a]
blocksP blockP = some blockP <* eof

intP :: Parser Int
intP = lexeme Lexer.decimal

signedIntP :: Parser Int
signedIntP = Lexer.signed sc intP

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

sc :: Parser ()
sc = Lexer.space space1 empty empty

enumParser ::
  forall a.
  (Bounded a, Enum a) =>
  (a -> String) ->
  (String -> Maybe a) ->
  Parser a
enumParser printer parser = do
  x <- choice $ fmap (string . printer) $ enumerate @a
  maybeToP parser x

maybeToP :: (a -> Maybe b) -> a -> Parser b
maybeToP f = maybe empty pure . f

predToP :: (a -> Bool) -> a -> Parser a
predToP p x = if p x then pure x else empty
