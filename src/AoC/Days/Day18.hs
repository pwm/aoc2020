module AoC.Days.Day18 where

import AoC.Lib.Parser
import AoC.Prelude
import Control.Monad.Combinators.Expr

parse :: String -> Maybe String
parse = Just

solveA :: String -> Int
solveA = maybe 0 (sum . fmap eval) . parseFileWith (exprP opTableA)

solveB :: String -> Int
solveB = maybe 0 (sum . fmap eval) . parseFileWith (exprP opTableB)

eval :: Expr -> Int
eval = \case
  Num a -> a
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving stock (Show, Eq, Ord, Generic)

exprP :: [[Operator Parser Expr]] -> Parser Expr
exprP opTable = makeExprParser ((Num <$> intP) <|> parensP (exprP opTable)) opTable

opTableA, opTableB :: [[Operator Parser Expr]]
opTableA = [[binaryL "+" Add, binaryL "*" Mul]]
opTableB = [[binaryL "+" Add], [binaryL "*" Mul]]

binaryL :: String -> (a -> a -> a) -> Operator Parser a
binaryL n s = InfixL $ s <$ symbol n
