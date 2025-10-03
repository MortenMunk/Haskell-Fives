module Player where

import Tile

newtype Hand = Hand [Tile]
  deriving (Show)
