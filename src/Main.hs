module Main where

import Boneyard
import Game (dealTwoHands, initBoneyard)
import Player
import Tile

main :: IO ()
main = do
  Boneyard shuffled <- initBoneyard
  let (hand1, hand2, boneyard) = dealTwoHands shuffled
      player = Player hand1 0
      enemy = Player hand2 0
  print (length (hand player))
  print (length (hand enemy))
  print (length boneyard)
