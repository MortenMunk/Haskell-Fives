{-# LANGUAGE DeriveFoldable #-}

module Player where

import Tile

newtype Hand a = Hand [a]
  deriving (Show, Eq, Foldable)

data Player = Player {hand :: Hand Tile, score :: Int}
  deriving (Show, Eq)

pickTile :: Hand Tile -> IO Tile
pickTile (Hand tiles) = do
  putStrLn "Your hand: "
  mapM_ (\(i, t) -> putStrLn (show i ++ ": " ++ show t)) (zip [1 ..] tiles)
  putStrLn "Pick tile by number:"
  input <- getLine
  let idx = read input :: Int
  return (tiles !! (idx - 1))
