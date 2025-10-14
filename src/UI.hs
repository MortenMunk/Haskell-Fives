module UI (runUI) where

import Board
import Boneyard
import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import qualified Graphics.Vty as V
import Player
import Tile

data St = St {player :: Player, enemy :: Player, board :: Board, boneyard :: Boneyard Tile}

drawUI :: St -> [Widget ()]
drawUI _ =
  [ padAll
      1
      ( borderWithLabel
          (str " DOMINO FIVES ")
          ( vBox
              [ hBox
                  [ fill ' ',
                    legendBox
                  ],
                center (str "Game goes here...")
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
