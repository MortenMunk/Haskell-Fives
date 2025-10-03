{-# LANGUAGE DeriveFoldable #-}

module Boneyard where

import Tile

newtype Boneyard a = Boneyard [a]
  deriving (Show, Foldable)
