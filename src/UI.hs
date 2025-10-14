module UI (runUI) where

import Board
import Boneyard
import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Center (center, hCenter)
import qualified Graphics.Vty as V
import Player
import Tile

data St = St {player :: Player, enemy :: Player, board :: Board, boneyard :: Boneyard Tile}

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
                  (center (str "Game goes here...")),
                padTop (Pad 1) hBorder,
                padTop (Pad 1) (hCenter (renderHand (hand (player s))))
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

renderHand :: Hand Tile -> Widget ()
renderHand (Hand tiles) =
  hBox (map (padRight (Pad 1) . renderTile) tiles)

handleEvent :: BrickEvent () e -> EventM n s ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = return ()

app :: App St e ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const (attrMap V.defAttr [])
    }

runUI :: Player -> Player -> Board -> Boneyard Tile -> IO ()
runUI p e b by = do
  _ <- defaultMain app (St p e b by)
  pure ()
