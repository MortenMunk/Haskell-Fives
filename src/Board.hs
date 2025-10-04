module Board where

import Game
import Tile

data Ends = TwoEnds Eye Eye | FourEnds Eye Eye Eye Eye
  deriving (Show)

data Board = Board {ends :: Ends, tiles :: [Tile]}
  deriving (Show)

emptyBoard :: Tile -> Board
emptyBoard (Tile l r) =
  if isDoubleTile (Tile l r)
    then Board (FourEnds l l l l) [Tile l r]
    else Board (TwoEnds l r) [Tile l r]
