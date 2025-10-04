module Game where

import Board
import Boneyard
import Data.List (maximumBy)
import Data.Ord (comparing)
import Player
import System.Random.Shuffle (shuffleM)
import Tile

data Turn = PlayerTurn | EnemyTurn
  deriving (Eq, Show)

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

pickFirstTurn :: (Player, Tile) -> (Player, Tile) -> Turn
pickFirstTurn (p1, t1) (p2, t2)
  | isDoubleTile t1 && not (isDoubleTile t2) = PlayerTurn
  | isDoubleTile t2 && not (isDoubleTile t1) = EnemyTurn
  | tileValue t1 > tileValue t2 = PlayerTurn
  | otherwise = EnemyTurn
  where
    tileValue (Tile l r) = fromEnum l + fromEnum r

playFirstTurn :: Hand Tile -> Board
playFirstTurn h = emptyBoard (getHighestTile h)

playNextTurn :: Hand Tile -> Board -> IO Board
playNextTurn hand board = do
  tile <- pickTile hand board
  case placeTile tile board of
    Just newBoard -> do
      putStrLn "Tile placed!"
      return newBoard
    Nothing -> do
      putStrLn "Illegal move - Try again!"
      playNextTurn hand board
