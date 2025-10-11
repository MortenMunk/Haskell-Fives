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

playNextTurn :: Hand Tile -> Board -> IO (Tile, Board)
playNextTurn hand board = do
  tile <- pickTile hand board
  case placeTile tile board of
    Just newBoard -> do
      putStrLn "Tile placed!"
      return (tile, newBoard)
    Nothing -> do
      putStrLn "Illegal move - Try again!"
      playNextTurn hand board

gameLoop :: Player -> Player -> Board -> Boneyard Tile -> IO ()
gameLoop player enemy board boneyard = do
  putStrLn "YOUR TURN!"
  print board

  let playable = legalTiles (hand player) board
  (player', board', boneyard') <-
    if null playable
      then drawUntilPlayable player boneyard board
      else playPlayerTurn player board boneyard

  if null (handTiles (hand player'))
    then putStrLn "You win!"
    else do
      putStrLn "ENEMY TURN!"
      (enemy', board'', boneyard'') <- playEnemyPhase enemy board' boneyard'
      if null (handTiles (hand enemy'))
        then putStrLn "Enemy wins!"
        else gameLoop player' enemy' board'' boneyard''

handTiles :: Hand Tile -> [Tile]
handTiles (Hand ts) = ts

playPlayerTurn :: Player -> Board -> Boneyard Tile -> IO (Player, Board, Boneyard Tile)
playPlayerTurn (Player (Hand tiles) score) board boneyard = do
  (tile, newBoard) <- playNextTurn (Hand tiles) board
  let remaining = filter (/= tile) tiles
  return (Player (Hand remaining) score, newBoard, boneyard)

drawUntilPlayable :: Player -> Boneyard Tile -> Board -> IO (Player, Board, Boneyard Tile)
drawUntilPlayable player (Boneyard []) board = do
  putStrLn "No playable tiles and boneyard empty - you pass."
  return (player, board, Boneyard [])
drawUntilPlayable (Player (Hand tiles) score) (Boneyard (t : ts)) board = do
  putStrLn ("You draw: " ++ show t)
  let newHand = Hand (t : tiles)
  if null (legalTiles newHand board)
    then drawUntilPlayable (Player newHand score) (Boneyard ts) board
    else playPlayerTurn (Player newHand score) board (Boneyard ts)

playEnemyPhase :: Player -> Board -> Boneyard Tile -> IO (Player, Board, Boneyard Tile)
playEnemyPhase (Player hand score) board boneyard =
  if null (legalTiles hand board)
    then drawEnemy (Player hand score) boneyard board
    else do
      (newBoard, newHand) <- playEnemyTurn hand board
      return (Player newHand score, newBoard, boneyard)

drawEnemy :: Player -> Boneyard Tile -> Board -> IO (Player, Board, Boneyard Tile)
drawEnemy (Player hand score) (Boneyard []) board = do
  putStrLn "Enemy cannot play and boneyard empty - passes"
  return (Player hand score, board, Boneyard [])
drawEnemy (Player (Hand tiles) score) (Boneyard (t : ts)) board = do
  putStrLn ("Enemy draws: " ++ show t)
  let newHand = Hand (t : tiles)
  if null (legalTiles newHand board)
    then drawEnemy (Player newHand score) (Boneyard ts) board
    else do
      (newBoard, newHand') <- playEnemyTurn newHand board
      return (Player newHand' score, newBoard, Boneyard ts)
