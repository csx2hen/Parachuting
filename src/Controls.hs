module Controls where
import Parachuting (Game (..), Tick(..), Name(..), direction, Direction (..), step, player, shouldRight, shouldLeft, moveSingleStep)
import Brick ( continue, halt, EventM, BrickEvent(VtyEvent, AppEvent), Next)
import qualified Graphics.Vty as V
import Control.Lens ((^.), (&), (.~), (%~))
import Prelude hiding (Right, Left)
import Linear (V2(V2))

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))        = continue $ handleDirection Left g
handleEvent g (VtyEvent (V.EvKey V.KRight []))       = continue $ handleDirection Right g
handleEvent g (VtyEvent (V.EvKey V.KUp []))          = continue $ handleDirection Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))        = continue $ handleDirection Down g 
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))  = continue $ handleDirection Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))  = continue $ handleDirection Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))  = continue $ handleDirection Up g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))  = continue $ handleDirection Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g _ = continue g

handleEventStep :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventStep g (AppEvent Tick)                        = continue $ step g
handleEventStep g (VtyEvent (V.EvKey V.KLeft []))        = continue $ moveSingleStep Left g
handleEventStep g (VtyEvent (V.EvKey V.KRight []))       = continue $ moveSingleStep Right g
handleEventStep g (VtyEvent (V.EvKey V.KUp []))          = continue $ moveSingleStep Up g
handleEventStep g (VtyEvent (V.EvKey V.KDown []))        = continue $ moveSingleStep Down g 
handleEventStep g (VtyEvent (V.EvKey (V.KChar 'a') []))  = continue $ moveSingleStep Left g
handleEventStep g (VtyEvent (V.EvKey (V.KChar 'd') []))  = continue $ moveSingleStep Right g
handleEventStep g (VtyEvent (V.EvKey (V.KChar 'w') []))  = continue $ moveSingleStep Up g
handleEventStep g (VtyEvent (V.EvKey (V.KChar 's') []))  = continue $ moveSingleStep Down g
handleEventStep g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEventStep g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEventStep g _ = continue g



oppositeDirection :: Direction -> Direction
oppositeDirection Left  = Right
oppositeDirection Right = Left
oppositeDirection Up    = Down
oppositeDirection Down  = Up
oppositeDirection Still = Still

handleDirection :: Direction -> Game -> Game
handleDirection dir g = 
    let opDir = oppositeDirection (g^.direction)
    in
        if opDir == dir
        then changeDir Still g
        else 
            if dir /= g^.direction
            then changeDir dir g
            else g

-- Not used
handleLeft :: Game -> Game
handleLeft = changeDir Left

handleRight :: Game -> Game
handleRight = changeDir Right

handleUp :: Game -> Game
handleUp = changeDir Right

handleDown :: Game -> Game
handleDown = changeDir Right

changeDir :: Direction -> Game -> Game
changeDir d g = g & direction .~ d