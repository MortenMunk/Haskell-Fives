module UI (runUI) where

import Board
import Boneyard
import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Center (center, hCenter)
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Player
import Tile

data St = St {player :: Player, enemy :: Player, board :: Board, boneyard :: Boneyard Tile, selected :: Int}

data TileOrientation = HorizontalTile | VerticalTile

selectedAttr, normalAttr :: AttrName
selectedAttr = attrName "selected"
normalAttr = attrName "normal"

drawUI :: St -> [Widget ()]
drawUI s =
  [ padAll
      1
      ( borderWithLabel
          (str " DOMINO FIVES ")
          ( vBox
              [ hBox
                  [ fill ' ',
                    legendBox
                  ],
                padTop
                  (Pad 1)
                  ( center
                      ( vBox
                          [ renderBoard (board s)
                          ]
                      )
                  ),
                padTop (Pad 1) hBorder,
                padTop
                  (Pad 1)
                  ( hCenter
                      (renderHand (hand (player s)) (selected s))
                  )
              ]
          )
      )
  ]

legendBox :: Widget ()
legendBox =
  hLimit
    35
    ( vLimit
        5
        ( border
            ( padLeftRight
                1
                ( vBox
                    [ str "Press q to quit",
                      str "Use arrows to move",
                      str "Enter to select tile"
                    ]
                )
            )
        )
    )

renderTile :: TileOrientation -> Tile -> Widget ()
renderTile HorizontalTile (Tile l r) =
  vLimit 3 $
    hLimit 9 $
      border $
        hBox
          [ padLeftRight 1 (str (show (fromEnum l))),
            vBorder,
            padLeftRight 1 (str (show (fromEnum r)))
          ]
renderTile VerticalTile (Tile l r) =
  hLimit 9 $
    vLimit 3 $
      border $
        vBox
          [ hCenter (str (show (fromEnum l))),
            hBorder,
            hCenter (str (show (fromEnum r)))
          ]

renderHand :: Hand Tile -> Int -> Widget ()
renderHand (Hand tiles) selectedIndex =
  hBox (zipWith renderIndexedTile [0 ..] tiles)
  where
    renderIndexedTile i tile =
      let w = renderTile HorizontalTile tile
       in if i == selectedIndex
            then withAttr selectedAttr w
            else withAttr normalAttr w

renderBoardTile :: Bool -> Tile -> Widget ()
renderBoardTile isVertical tile =
  if isVertical
    then renderTile VerticalTile tile
    else renderTile HorizontalTile tile

renderBoard :: Board -> Widget ()
renderBoard (Board ends tilesPlayed) =
  vBox
    [ hBox (map (renderTile HorizontalTile) (reverse tilesPlayed))
    ]

handleEvent :: BrickEvent () e -> EventM n St ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) =
  modify (\st -> st {selected = max 0 (selected st - 1)})
handleEvent (VtyEvent (V.EvKey V.KRight [])) =
  modify
    ( \st ->
        let Hand ts = hand (player st)
         in st {selected = min (length ts - 1) (selected st + 1)}
    )
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let Hand tiles = hand (player st)
      chosen = tiles !! selected st
      playable = legalTiles (hand (player st)) (board st)
  if chosen `notElem` playable
    then liftIO (putStrLn "invalid move") >> pure ()
    else case placeTile chosen (board st) of
      Nothing -> liftIO (putStrLn "invalid placement") >> pure ()
      Just newBoard -> do
        let newHand = removeTile chosen (hand (player st))
            newPlayer = (player st) {hand = newHand}
            baseSt = st {board = newBoard, player = newPlayer, selected = 0}

        (newBoard', newEnemyHand) <- liftIO $ playEnemyTurn (hand (enemy st)) newBoard

        put
          baseSt
            { board = newBoard',
              enemy =
                (enemy st)
                  { hand = newEnemyHand
                  }
            }
  pure ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = return ()

app :: App St e ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap =
        const
          ( attrMap
              V.defAttr
              [ (selectedAttr, V.black `on` V.yellow),
                (normalAttr, V.defAttr)
              ]
          )
    }

runUI :: Player -> Player -> Board -> Boneyard Tile -> IO ()
runUI p e b by = do
  _ <- defaultMain app (St p e b by 0)
  pure ()
