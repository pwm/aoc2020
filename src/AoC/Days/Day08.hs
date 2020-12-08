module AoC.Days.Day08 where

import AoC.Lib.Parser
import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Set qualified as Set

parse :: String -> Maybe Program
parse = fmap (Map.fromList . zip [0 ..]) . parseFileWith opP

solveA :: Program -> Int
solveA = view #counter . runComputer

solveB :: Program -> Int
solveB = fromMaybe 0 . preview (_Just . #counter) . findWorkingProgram . genPrograms

findWorkingProgram :: [Program] -> Maybe Memory
findWorkingProgram [] = Nothing
findWorkingProgram (p : ps)
  | not (m ^. #looping) = Just m
  | otherwise = findWorkingProgram ps
  where
    m = runComputer p

genPrograms :: Program -> [Program]
genPrograms p =
  fmap (`flipNopJmpAt` p) $ Map.keys $ Map.filter ((`elem` [Jmp, Nop]) . fst) p
  where
    flipNopJmpAt :: Int -> Program -> Program
    flipNopJmpAt = Map.adjust $ \case
      (Nop, x) -> (Jmp, x)
      (Jmp, x) -> (Nop, x)
      x -> x

runComputer :: Program -> Memory
runComputer = execState computer . loadProgram

computer :: (MonadState Memory m) => m ()
computer = do
  c <- get
  case (c ^. #program) !? (c ^. #ip) of
    Nothing -> pure ()
    Just nextOp -> do
      ip <- use #ip
      ipLog <- use #ipLog
      if Set.member ip ipLog
        then #looping .= True
        else do
          #ipLog %= Set.insert ip
          #opLog %= (|> nextOp)
          #ip += 1
          execOp nextOp
      looping <- use #looping
      unless looping computer

execOp :: (MonadState Memory m) => Op -> m ()
execOp = \case
  (Acc, x) -> #counter += x
  (Jmp, x) -> #ip += (x - 1)
  (Nop, _) -> pure ()

loadProgram :: Program -> Memory
loadProgram p =
  MkMemory
    { program = p,
      ip = 0,
      ipLog = mempty,
      opLog = mempty,
      counter = 0,
      looping = False
    }

data Memory = MkMemory
  { program :: Program,
    ip :: Int,
    ipLog :: Set Int,
    opLog :: Seq Op,
    counter :: Int,
    looping :: Bool
  }
  deriving stock (Show, Eq, Generic)

type Op = (TyOp, Int)

type Program = Map Int Op

data TyOp
  = Acc
  | Jmp
  | Nop
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

opP :: Parser Op
opP = (,) <$> tyOpP <* char ' ' <*> signedIntP
  where
    tyOpP :: Parser TyOp
    tyOpP = enumParser (fmap toLower . show) $ \case
      "acc" -> Just Acc
      "jmp" -> Just Jmp
      "nop" -> Just Nop
      _ -> Nothing
