module Controller where
import Core (Game (..), Tick(..), Name(..), direction, Direction (..), step, player, shouldRight, shouldLeft, movePlayerSingleStep, initState, maxDepth, Movement (Left, Right), changeDir)
import Brick ( continue, halt, EventM, BrickEvent(VtyEvent, AppEvent), Next)
import qualified Graphics.Vty as V
import Control.Lens ((^.), (&), (.~), (%~))
import Prelude hiding (Right, Left)
import Linear (V2(V2))
import Control.Monad.IO.Class (liftIO)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
-- handleEventStep g (VtyEvent (V.EvKey (V.KChar 'a') []))  = continue $ moveSingleStep Left g
-- handleEventStep g (VtyEvent (V.EvKey (V.KChar 'd') []))  = continue $ moveSingleStep Right g
-- handleEventStep g (VtyEvent (V.EvKey (V.KChar 'w') []))  = continue $ moveSingleStep Up g
-- handleEventStep g (VtyEvent (V.EvKey (V.KChar 's') []))  = continue $ moveSingleStep Down g
handleEvent g (AppEvent Tick)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))        = continue $ movePlayerSingleStep Left g
handleEvent g (VtyEvent (V.EvKey V.KRight []))       = continue $ movePlayerSingleStep Right g
handleEvent g (VtyEvent (V.EvKey V.KUp []))          = continue $ handleDirection Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))        = continue $ handleDirection Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') []))  = liftIO (initState (g^.maxDepth)) >>= continue
handleEvent g _ = continue g

oppositeDirection :: Direction -> Direction
oppositeDirection Up    = Down
oppositeDirection Down  = Up
oppositeDirection Still = Still

handleDirection :: Direction -> Game -> Game
handleDirection dir g =
    let opDir = oppositeDirection (g^.direction)
    in
        if opDir == dir
        then
            changeDir Still g
        else
            changeDir dir g
