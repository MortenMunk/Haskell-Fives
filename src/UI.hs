module UI (runUI) where

import Board
import Boneyard
import Brick
import qualified Graphics.Vty as V
import Player
import Tile

data St = St {player :: Player, enemy :: Player, board :: Board, boneyard :: Boneyard Tile}

drawUI :: St -> [Widget ()]
drawUI s =
  [ vBox
      [str "=== DOMINO FIVES ===", padTop (Pad 1) (str "Press q to quit.")]
  ]

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
