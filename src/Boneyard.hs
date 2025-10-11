{-# LANGUAGE DeriveFoldable #-}

module Boneyard where

import Tile

newtype Boneyard a = Boneyard [a]
  deriving (Show, Foldable)

drawTile :: Boneyard Tile -> Maybe (Tile, Boneyard Tile)
drawTile (Boneyard []) = Nothing
drawTile (Boneyard (t : ts)) = Just (t, Boneyard ts)
