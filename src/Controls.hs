module Controls where
import Parachuting (Game (..), Tick(..), Name(..), direction, Direction (..), step, player, shouldRight, shouldLeft)
import Brick ( continue, halt, EventM, BrickEvent(VtyEvent, AppEvent), Next )
import qualified Graphics.Vty as V
import Control.Lens ((^.), (&), (.~), (%~))
import Prelude hiding (Right, Left)
import Linear (V2(V2))

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))        = continue $ handleLeft g
handleEvent g (VtyEvent (V.EvKey V.KRight []))       = continue $ handleRight g
handleEvent g (VtyEvent (V.EvKey V.KUp []))          = continue $ handleUp g
handleEvent g (VtyEvent (V.EvKey V.KDown []))        = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))  = continue $ handleLeft g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))  = continue $ handleRight g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))  = continue $ handleUp g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))  = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g _ = continue g

handleLeft :: Game -> Game
handleLeft g = if shouldLeft g
               then g & player %~ fmap (+ V2 (-1) 0)
               else g

handleRight :: Game -> Game
handleRight g = if shouldRight g
                then g & player %~ fmap (+ V2 1 0)
                else g

changeDir :: Direction -> Game -> Game
changeDir d g = g & direction .~ d

handleUp :: Game -> Game
handleUp g = if g^.direction /= Still
             then changeDir Still g
             else g

handleDown :: Game -> Game
handleDown g =  if g^.direction /= Down
                then changeDir Down g
                else g
