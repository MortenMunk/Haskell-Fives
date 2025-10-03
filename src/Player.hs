{-# LANGUAGE DeriveFoldable #-}

module Player where

import Tile

newtype Hand a = Hand [a]
  deriving (Show, Eq, Foldable)

data Player = Player {hand :: Hand Tile, score :: Int}
  deriving (Show, Eq)
