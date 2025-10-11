{-# LANGUAGE DeriveFoldable #-}

module Player where

import Board
import Data.List (maximumBy)
import Data.Ord (comparing)
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
  case reads input :: [(Int, String)] of
    [(idx, _)]
      | idx >= 1 && idx <= length tiles -> return (tiles !! (idx - 1))
      | otherwise -> do
          putStrLn "Invalid number! Try again."
          pickTile (Hand tiles) board
    _ -> do
      putStrLn "Please enter a valid number!"
      pickTile (Hand tiles) board

canPlace :: Tile -> Board -> Bool
canPlace tile board = case placeTile tile board of
  Just _ -> True
  Nothing -> False

legalTiles :: Hand Tile -> Board -> [Tile]
legalTiles (Hand tiles) board = [t | t <- tiles, canPlace t board]

playEnemyTurn :: Hand Tile -> Board -> IO (Board, Hand Tile)
playEnemyTurn hand@(Hand tiles) board = do
  let tile = maximumBy (comparing tileValue) (legalTiles hand board)
  putStrLn ("Enemy plays: " ++ show tile)
  let Just newBoard = placeTile tile board
      remaining = Hand (filter (/= tile) tiles)
  return (newBoard, remaining)
  where
    tileValue (Tile l r) = fromEnum l + fromEnum r

removeTile :: Tile -> Hand Tile -> Hand Tile
removeTile t (Hand ts) = Hand (filter (/= t) ts)
