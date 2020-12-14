module AoC.Lib.Memo where

import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map

type Memo k v = State (Map k v) v

memoMap :: (Ord k) => ((k -> Memo k v) -> k -> Memo k v) -> k -> v
memoMap f = flip evalState mempty . f go
  where
    go k = do
      m <- get
      case m !? k of
        Just v -> pure v
        _ -> do
          v <- f go k
          modify (Map.insert k v)
          pure v

noMemo :: ((a -> Identity b) -> a -> Identity b) -> a -> b
noMemo f = runIdentity . fix f
