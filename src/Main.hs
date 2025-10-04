module Main where

import Board
import Boneyard
import Game
import Player
import Tile

main :: IO ()
main = do
  Boneyard shuffled <- initBoneyard
  let (hand1, hand2, boneyard) = dealTwoHands shuffled
      player = Player hand1 0
      enemy = Player hand2 0
      starter = pickFirstTurn (player, getHighestTile (hand player)) (enemy, getHighestTile (hand enemy))

      firstMove =
        if starter == player
          then getHighestTile (hand player)
          else getHighestTile (hand enemy)
      board = emptyBoard firstMove

  print board
