{-# LANGUAGE DeriveFoldable #-}

module Player where

import Board
import Tile

newtype Hand a = Hand [a]
  deriving (Show, Eq, Foldable)

data Player = Player {hand :: Hand Tile, score :: Int}
  deriving (Show, Eq)

pickTile :: Hand Tile -> Board -> IO Tile
pickTile (Hand tiles) board = do
  let playable = legalTiles (Hand tiles) board

  putStrLn "Your hand: "
  mapM_ (\(i, t) -> putStrLn (show i ++ ": " ++ show t ++ if t `elem` playable then " ✅" else "")) (zip [1 ..] tiles)
  putStrLn "Pick tile by number:"
  input <- getLine
  let idx = read input :: Int
  return (tiles !! (idx - 1))

-- playEnemyTurn :: Hand Tile -> Board -> IO (Maybe Board)
-- playEnemyTurn tiles board =

canPlace :: Tile -> Board -> Bool
canPlace tile board = case placeTile tile board of
  Just _ -> True
  Nothing -> False

legalTiles :: Hand Tile -> Board -> [Tile]
legalTiles (Hand tiles) board = [t | t <- tiles, canPlace t board]
