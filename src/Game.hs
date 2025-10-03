module Game where

import Boneyard
import Data.List (maximumBy)
import Data.Ord (comparing)
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

getHighestTile :: Hand Tile -> Tile
getHighestTile (Hand tiles) =
  case filter isDoubleTile tiles of
    [] -> maximumBy (comparing tileValue) tiles
    dubs -> maximumBy (comparing tileValue) dubs
  where
    tileValue (Tile l r) = fromEnum l + fromEnum r

isDoubleTile :: Tile -> Bool
isDoubleTile (Tile l r) = l == r

pickFirstTurn :: (Player, Tile) -> (Player, Tile) -> Player
pickFirstTurn (p1, t1) (p2, t2)
  | isDoubleTile t1 && not (isDoubleTile t2) = p1
  | isDoubleTile t2 && not (isDoubleTile t1) = p2
  | tileValue t1 > tileValue t2 = p1
  | otherwise = p2
  where
    tileValue (Tile l r) = fromEnum l + fromEnum r
