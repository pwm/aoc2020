module AoC.Lib.Solver where

import AoC.Lib.Day
import AoC.Lib.File
import AoC.Prelude
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map

solveDay :: [Day -> IO ()] -> Day -> IO ()
solveDay sols day = case solMap sols !? day of
  Nothing -> error $ "Day " <> displayDay day <> " does not yet exists."
  Just solver -> solver day
  where
    solMap :: [a] -> Map Day a
    solMap = Map.fromList . fmap (first UnsafeMkDay) . zip [1 ..]

mkSolver ::
  (Show a, Show b) =>
  (String -> Maybe i) ->
  (i -> a) ->
  (i -> b) ->
  Day ->
  IO ()
mkSolver parser solverA solverB day = do
  file <- loadInputFile day
  case parser file of
    Nothing -> error ("Cannot parse input file " <> displayDayFile day)
    Just input -> print (solverA input, solverB input)
