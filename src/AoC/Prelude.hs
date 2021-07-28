module AoC.Prelude
  ( module X,
    -- base Prelude
    String,
    lines,
    unlines,
    words,
    unwords,
    readFile,
    -- custom
    error,
    success,
    unsafePrint,
    unsafePutStrLn,
    fixpoint,
    fixpointM,
    headOr,
    dropEnd,
    enumerate,
    hasKeys,
    rsort,
    charAt,
    l2p,
    getDataFileName,
    pick,
    slicesOf,
    lookups,
    compose,
    applyTimes,
    composeM,
    applyTimesM,
    substring,
    tupleMin,
    tupleMax,
    binToDec,
    sqrtInt,
    choose,
  )
where

import Control.Lens as X hiding (inside, op, (<.>), (<|), (|>))
import Control.Monad.State.Strict as X
import Data.Bits as X
import Data.Either as X
import Data.Generics.Labels as X ()
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe as X hiding (fromJust)
import Data.Set qualified as Set
import Paths_aoc2020 (getDataFileName)
import Protolude as X hiding (State, StateT, evalState, evalStateT, execState, execStateT, from, lines, many, option, readFile, runState, runStateT, some, to, try, uncons, unlines, unsnoc, unwords, withState, words)
import System.IO.Unsafe (unsafePerformIO)
import Prelude (String, lines, readFile, unlines, unwords, words)

error, success :: String -> IO ()
error e = print e >> exitFailure
success v = print v >> exitSuccess

unsafePrint :: (Show a) => a -> ()
unsafePrint = unsafePerformIO . print

unsafePutStrLn :: (Print a) => a -> ()
unsafePutStrLn = unsafePerformIO . putStrLn

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x = if x == f x then x else fixpoint f (f x)

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f x = do
  y <- f x
  if x == y then pure y else fixpointM f y

-- headOr 0 [] -> 0
-- headOr 0 [1, 2, 3] -> 1
headOr :: a -> [a] -> a
headOr x = fromMaybe x . head

-- dropEnd 1 [1..3] -> [1, 2]
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

-- enumerate @Bool -> [False,True]
enumerate :: forall a. (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

-- {a, b} -> [(a, 1), (b, 2), (c, 3)] -> True
-- {a, b, c} -> [(a, 1), (b, 2)] -> False
hasKeys :: (Ord a) => Set a -> Map a b -> Bool
hasKeys keys = Set.isSubsetOf keys . Map.keysSet

-- [2,3,1,2] -> [3,2,2,1]
rsort :: (Ord a) => [a] -> [a]
rsort = sortOn Down

-- 2 "abcd" -> Just 'c'
-- 5 "abcd" -> Nothing
charAt :: Int -> String -> Maybe Char
charAt x = fmap fst . uncons . drop x

-- [1, 2] -> Just (1, 2)
-- [1, 2, 3] -> Nothing
l2p :: [a] -> Maybe (a, a)
l2p [a, b] = Just (a, b)
l2p _ = Nothing

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = fmap (x :) (pick (k - 1) xs) <> pick k xs

slicesOf :: Int -> [a] -> [[a]]
slicesOf n = unfoldr $ \xs ->
  let (s, t) = (take n xs, drop 1 xs)
   in if length s >= n then Just (s, t) else Nothing

lookups :: (Ord k) => Map k v -> [k] -> [v]
lookups g = mapMaybe (g !?)

compose :: (Foldable t) => t (b -> b) -> b -> b
compose = foldr (.) identity

applyTimes :: Int -> (b -> b) -> b -> b
applyTimes n = compose . replicate n

composeM :: (Foldable t, Monad m) => t (b -> m b) -> b -> m b
composeM = foldr (<=<) pure

applyTimesM :: (Monad m) => Int -> (b -> m b) -> b -> m b
applyTimesM n = composeM . replicate n

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

tupleMin, tupleMax :: (Ord a, Each s s a a) => s -> a
tupleMin = minimum . toListOf each
tupleMax = maximum . toListOf each

binToDec :: [Bool] -> Integer
binToDec = foldl' (\acc x -> 2 * acc + toInteger (fromEnum x)) 0

sqrtInt :: Int -> Int
sqrtInt = floor @Double . sqrt . fromIntegral

choose :: (Traversable t, Alternative m) => t a -> m a
choose = asum . fmap pure
