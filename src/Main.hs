module Main where

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

      firstBoard =
        if starter == player
          then playFirstTurn (hand player)
          else playFirstTurn (hand enemy)

  -- TEMPORARY
  let gameOver = False
  print firstBoard
  if not gameOver
    then do
      nextBoard <- playNextTurn (hand player) firstBoard
      print nextBoard
    else print "Game Over!"
