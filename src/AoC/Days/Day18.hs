module AoC.Days.Day18 where

import AoC.Lib.Parser
import AoC.Prelude
import Control.Monad.Combinators.Expr

parse :: String -> Maybe String
parse = Just

solveA, solveB :: String -> Int
solveA = solve opTableA
solveB = solve opTableB

solve :: [[Operator Parser Expr]] -> String -> Int
solve opTbl = maybe 0 (sum . fmap eval) . parseMaybe (some (exprP opTbl) <* eof)

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving stock (Show, Eq, Ord, Generic)

eval :: Expr -> Int
eval = \case
  Num a -> a
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b

exprP :: [[Operator Parser Expr]] -> Parser Expr
exprP opTable = makeExprParser ((Num <$> intP) <|> parensP (exprP opTable)) opTable

opTableA, opTableB :: [[Operator Parser Expr]]
opTableA = [[binaryL "+" Add, binaryL "*" Mul]]
opTableB = [[binaryL "+" Add], [binaryL "*" Mul]]

binaryL :: String -> (a -> a -> a) -> Operator Parser a
binaryL n s = InfixL $ s <$ symbol n
