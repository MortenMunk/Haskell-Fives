module Main where

import Boneyard
import Game
import Player
import Tile
import UI

main :: IO ()
main = do
  Boneyard shuffled <- initBoneyard
  let (hand1, hand2, boneyard) = dealTwoHands shuffled
      player = Player hand1 0
      enemy = Player hand2 0
      starter = pickFirstTurn (player, getHighestTile (hand player)) (enemy, getHighestTile (hand enemy))

      firstBoard = case starter of
        PlayerTurn -> playFirstTurn (hand player)
        EnemyTurn -> playFirstTurn (hand enemy)

      player' =
        if starter == PlayerTurn
          then Player (removeTile (getHighestTile hand1) hand1) 0
          else player

      enemy' =
        if starter == EnemyTurn
          then Player (removeTile (getHighestTile hand2) hand2) 0
          else player

  putStrLn ("\nStarter: " ++ show starter)
  print firstBoard

  -- gameLoop player' enemy' firstBoard boneyard
  runUI player' enemy' firstBoard boneyard
