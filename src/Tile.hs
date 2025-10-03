module Tile where

data Eye = Zero | One | Two | Three | Four | Five | Six
  deriving (Eq, Ord, Show, Enum, Bounded)

data Tile = Tile Eye Eye
  deriving (Eq, Show)
