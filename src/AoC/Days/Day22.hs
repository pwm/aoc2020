module AoC.Days.Day22 where

import AoC.Lib.Parser
import AoC.Prelude hiding (pattern Empty)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

parse :: String -> Maybe Game
parse = parseMaybe gameP

solveA :: Game -> Int
solveA = score . winner . combat

solveB :: Game -> Int
solveB = score . winner . recCombat

combat :: Game -> Game
combat = fixpoint doCombat
  where
    doCombat :: Game -> Game
    doCombat g@(p1, p2)
      | inGame g && h1 > h2 = (t1 |> h1 |> h2, t2)
      | inGame g && h1 < h2 = (t1, t2 |> h2 |> h1)
      | otherwise = (p1, p2)
      where
        (h1, t1) = drawCard p1
        (h2, t2) = drawCard p2

recCombat :: Game -> Game
recCombat = flip evalState mempty . fixpointM doRecCombat
  where
    doRecCombat :: (MonadState (Set Game) m) => Game -> m Game
    doRecCombat g@(p1, p2) = do
      gs <- get
      modify $ Set.insert g
      pure $
        if
            | Set.member g gs -> (p1, mempty)
            | inGame g && length t1 >= h1 && length t2 >= h2 ->
              if null $ snd $ recCombat (Seq.take h1 t1, Seq.take h2 t2)
                then (t1 |> h1 |> h2, t2)
                else (t1, t2 |> h2 |> h1)
            | inGame g && h1 > h2 -> (t1 |> h1 |> h2, t2)
            | inGame g && h1 < h2 -> (t1, t2 |> h2 |> h1)
            | otherwise -> (p1, p2)
      where
        (h1, t1) = drawCard p1
        (h2, t2) = drawCard p2

inGame :: Game -> Bool
inGame (p1, p2) = not (null p1) && not (null p2)

drawCard :: Seq Int -> (Int, Seq Int)
drawCard = \case
  Empty -> (0, mempty)
  h :<| t -> (h, t)

winner :: Game -> Seq Int
winner (p1, p2) = if null p2 then p1 else p2

score :: Seq Int -> Int
score = foldr (\(k, v) acc -> acc + k * v) 0 . zip [1 ..] . reverse . toList

type Game = (Seq Int, Seq Int)

gameP :: Parser Game
gameP = do
  _ <- lexeme "Player " <* digitChar <* lexeme ":"
  p1 <- some (intP <* sc)
  _ <- sc <* lexeme "Player " <* digitChar <* lexeme ":"
  p2 <- some (intP <* sc)
  pure (Seq.fromList p1, Seq.fromList p2)
