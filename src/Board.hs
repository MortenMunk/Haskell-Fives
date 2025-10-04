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
  | l == left = Just (Board (TwoEnds r right) newTiles)
  | r == left = Just (Board (TwoEnds l right) newTiles)
  | l == right = Just (Board (TwoEnds left r) newTiles)
  | r == right = Just (Board (TwoEnds left l) newTiles)
  | otherwise = Nothing
  where
    newTiles = Tile l r : ts
placeTile (Tile l r) (Board (FourEnds a b c d) ts)
  | l == a = Just (Board (FourEnds r b c d) newTiles)
  | r == a = Just (Board (FourEnds l b c d) newTiles)
  | l == b = Just (Board (FourEnds a r c d) newTiles)
  | r == b = Just (Board (FourEnds a l c d) newTiles)
  | l == c = Just (Board (FourEnds a b r d) newTiles)
  | r == c = Just (Board (FourEnds a b l d) newTiles)
  | l == d = Just (Board (FourEnds a b c r) newTiles)
  | r == d = Just (Board (FourEnds a b c l) newTiles)
  | otherwise = Nothing
  where
    newTiles = Tile l r : ts

boardScore :: Board -> Int
boardScore (Board (TwoEnds l r) _) = fromEnum l + fromEnum r
boardScore (Board (FourEnds a b c d) _) = fromEnum a + fromEnum b + fromEnum c + fromEnum d
