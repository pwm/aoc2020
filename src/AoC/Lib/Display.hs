module AoC.Lib.Display where

import AoC.Lib.Grid
import AoC.Prelude
import Data.Map.Strict qualified as Map
import System.IO qualified as SIO

displayGrid :: GridOf a -> (a -> String) -> IO ()
displayGrid grid drawCell = display grid (putStrLn . drawGrid drawCell)

display :: a -> (a -> IO ()) -> IO ()
display a draw = do
  SIO.hSetBuffering stdin SIO.NoBuffering
  SIO.hSetEcho stdin False
  putStrLn clrScr
  draw a

drawGrid :: forall a. (a -> String) -> GridOf a -> String
drawGrid drawCell = Map.foldrWithKey go ""
  where
    go :: Pos -> a -> String -> String
    go pos a screen = screen <> cursorTo pos <> drawCell a

clrScr :: String
clrScr = "\ESC[2J"

cursorTo :: Pos -> String
cursorTo (x, y) = "\ESC[" <> show (x + 1) <> ";" <> show (y + 1) <> "H"

colourAs :: Colour -> String -> String
colourAs c s = "\ESC[" <> colourToAnsi c <> ";1m" <> s <> "\ESC[0m"

data Colour
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Reset
  deriving stock (Show, Eq, Ord)

colourToAnsi :: Colour -> String
colourToAnsi = \case
  Black -> "30"
  Red -> "31"
  Green -> "32"
  Yellow -> "33"
  Blue -> "34"
  Magenta -> "35"
  Cyan -> "36"
  White -> "70"
  Reset -> "0"

getKey :: IO String
getKey = reverse <$> getKeyPress ""
  where
    getKeyPress chars = do
      char <- SIO.getChar
      more <- SIO.hReady stdin
      (if more then getKeyPress else pure) (char : chars)
