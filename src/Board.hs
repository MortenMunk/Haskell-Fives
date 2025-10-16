module Board where

import Tile

data BranchDirection = North | South | East | West
  deriving (Show, Eq)

data Board = Board
  { spinner :: Maybe Tile,
    branches :: [(BranchDirection, [Tile])]
  }
  deriving (Show)

emptyBoard :: Tile -> Board
emptyBoard t@(Tile l r)
  | isDoubleTile t =
      Board (Just t) [(North, []), (South, []), (East, []), (West, [])]
  | otherwise =
      Board Nothing [(East, [t]), (West, []), (North, []), (South, [])]

placeTile :: Tile -> Board -> Maybe Board
placeTile tile board =
  case matchingBranch tile board of
    Nothing -> Nothing
    Just dir -> Just (appendTo dir tile board)

matchingBranch :: Tile -> Board -> Maybe BranchDirection
matchingBranch (Tile l r) (Board mSp brs)
  | Just sp@(Tile sl sr) <- mSp,
    isDoubleTile sp =
      let connectsToEnd d =
            case lookup d brs of
              Just [] -> connectsTo sp (Tile l r)
              Just ts ->
                let Tile a b = last ts
                 in l == a || l == b || r == a || r == b
              Nothing -> False
       in case filter connectsToEnd [North, South, East, West] of
            (d : _) -> Just d
            [] -> Nothing
  | otherwise =
      let eastEnd = lastEye East brs mSp
          westEnd = lastEye West brs mSp
       in if l == eastEnd || r == eastEnd
            then Just East
            else
              if l == westEnd || r == westEnd
                then Just West
                else Nothing

appendTo :: BranchDirection -> Tile -> Board -> Board
appendTo dir t@(Tile l r) b@(Board mSp brs)
  | Just sp <- mSp, sp == t = b
  | otherwise =
      let update (d, ts)
            | d == dir =
                let endEye = lastEye d brs mSp
                    t' =
                      if endEye == l
                        then Tile l r
                        else
                          if endEye == r
                            then Tile r l
                            else Tile l r
                    newTiles =
                      case d of
                        North -> t' : ts
                        West -> t' : ts
                        South -> ts ++ [t']
                        East -> ts ++ [t']
                 in (d, newTiles)
            | otherwise = (d, ts)
       in Board mSp (map update brs)

lastEye :: BranchDirection -> [(BranchDirection, [Tile])] -> Maybe Tile -> Eye
lastEye dir brs mSp =
  case lookup dir brs of
    Just [] ->
      case mSp of
        Just (Tile l r) ->
          case dir of
            North -> l
            South -> l
            East -> r
            West -> r
        Nothing -> Zero
    Just ts -> let Tile _ r = last ts in r
    Nothing -> Zero

connectsTo :: Tile -> Tile -> Bool
connectsTo (Tile a b) (Tile c d) = a == c || a == d || b == c || b == d

boardScore :: Board -> Int
boardScore (Board mSp brs) =
  sum (map (fromEnum . (\d -> lastEye d brs mSp)) [North, South, East, West])
