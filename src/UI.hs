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
                          [ str "Board:",
                            str (show (board s))
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

renderPips :: Int -> Widget ()
renderPips n =
  let dot b = if b then "●" else " "
      row a b c = hCenter (hBox [str (dot a), str (dot b), str (dot c)])
   in vBox
        ( case n of
            0 ->
              [ row False False False,
                row False False False,
                row False False False
              ]
            1 ->
              [ row False False False,
                row False True False,
                row False False False
              ]
            2 ->
              [ row True False False,
                row False False False,
                row False False True
              ]
            3 ->
              [ row True False False,
                row False True False,
                row False False True
              ]
            4 ->
              [ row True False True,
                row False False False,
                row True False True
              ]
            5 ->
              [ row True False True,
                row False True False,
                row True False True
              ]
            6 ->
              [ row True False True,
                row True False True,
                row True False True
              ]
            _ ->
              [ row False False False,
                row False False False,
                row False False False
              ]
        )

renderTile :: Tile -> Widget ()
renderTile (Tile l r) =
  vLimit 5 $
    hLimit 13 $
      border
        ( hBox
            [ padLeftRight 1 (renderPips (fromEnum l)),
              vBorder,
              padLeftRight 1 (renderPips (fromEnum r))
            ]
        )

renderHand :: Hand Tile -> Int -> Widget ()
renderHand (Hand tiles) selectedIndex =
  hBox (zipWith renderIndexedTile [0 ..] tiles)
  where
    renderIndexedTile i tile =
      let w = renderTile tile
       in if i == selectedIndex
            then withAttr selectedAttr w
            else withAttr normalAttr w

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
  liftIO (putStrLn ("played " ++ show chosen))
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
