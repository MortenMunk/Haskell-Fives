module Board where

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

placeTile :: Tile -> Board -> Maybe Board
placeTile (Tile l r) (Board (TwoEnds left right) ts)
  | l == left = Just (Board (TwoEnds r right) tiles)
  | r == left = Just (Board (TwoEnds l right) tiles)
  | l == right = Just (Board (TwoEnds left r) tiles)
  | r == right = Just (Board (TwoEnds left l) tiles)
  | otherwise = Nothing
  where
    tiles = Tile l r : ts
placeTile (Tile l r) (Board (FourEnds a b c d) ts)
  | l == a = Just (Board (FourEnds r b c d) tiles)
  | r == a = Just (Board (FourEnds l b c d) tiles)
  | l == b = Just (Board (FourEnds a r c d) tiles)
  | r == b = Just (Board (FourEnds a l c d) tiles)
  | l == c = Just (Board (FourEnds a b r d) tiles)
  | r == c = Just (Board (FourEnds a b l d) tiles)
  | l == d = Just (Board (FourEnds a b c r) tiles)
  | r == d = Just (Board (FourEnds a b c l) tiles)
  | otherwise = Nothing
  where
    tiles = Tile l r : ts
