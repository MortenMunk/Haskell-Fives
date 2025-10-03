module Game where

import Boneyard
import Player
import System.Random.Shuffle (shuffleM)
import Tile

initBoneyard :: IO (Boneyard Tile)
initBoneyard = Boneyard <$> shuffleM allTiles

dealHand :: [Tile] -> (Hand Tile, Boneyard Tile)
dealHand tiles =
  let (h, rest) = splitAt 7 tiles
   in (Hand h, Boneyard rest)

dealTwoHands :: [Tile] -> (Hand Tile, Hand Tile, Boneyard Tile)
dealTwoHands tiles =
  let (hand1, Boneyard rest) = dealHand tiles
      (hand2, boneyard) = dealHand rest
   in (hand1, hand2, boneyard)
