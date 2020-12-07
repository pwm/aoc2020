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
    headOr,
    enumerate,
    hasKeys,
    rsort,
    charAt,
    l2p,
    getDataFileName,
  )
where

import Control.Lens as X hiding ((<.>))
import Data.Bits as X
import Data.Either as X
import Data.Generics.Labels as X ()
import Data.Map.Strict qualified as Map
import Data.Maybe as X hiding (fromJust)
import Data.Set qualified as Set
import Paths_aoc2020 (getDataFileName)
import Protolude as X hiding (from, lines, many, option, readFile, some, to, try, uncons, unlines, unsnoc, unwords, words)
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

-- headOr 0 [] -> 0
-- headOr 0 [1, 2, 3] -> 1
headOr :: a -> [a] -> a
headOr x = fromMaybe x . head

-- enumerate @Bool -> [False,True]
enumerate :: forall a. (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

-- {a, b} -> [(a, 1), (b, 2), (c, 3)] -> True
-- {a, b, c} -> [(a, 1), (b, 2)] -> False
hasKeys :: Ord a => Set a -> Map a b -> Bool
hasKeys keys = Set.isSubsetOf keys . Map.keysSet

-- [2,3,1,2] -> [3,2,2,1]
rsort :: Ord a => [a] -> [a]
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
