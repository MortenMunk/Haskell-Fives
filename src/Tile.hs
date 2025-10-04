module Tile where

data Eye = Zero | One | Two | Three | Four | Five | Six
  deriving (Eq, Ord, Show, Enum, Bounded)

data Tile = Tile Eye Eye
  deriving (Eq, Show)

allTiles :: [Tile]
allTiles = [Tile l r | l <- [Zero .. Six], r <- [Zero .. Six], l >= r]

isDoubleTile :: Tile -> Bool
isDoubleTile (Tile l r) = l == r
