module UI where

import Core
    ( Game,
      Tick(..),
      Depth,
      Name,
      alive,
      depth,
      maxDepth,
      obstacles,
      player,
      gridWidth,
      gridHeight,
      initState, inJellyFish, inMine, inLeftShark, inRightShark, Mode, mode, inDark
    )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.BChan as BChan
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Lens.Micro ((^.), mapped)
import Linear.V2 (V2(..))
import Control.Monad ( forever )
import Brick.Util ( fg, on )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import GHC.IO (unsafePerformIO)
import Controller(handleEvent)

import Brick
  ( App(..), BrickEvent(..), Padding(..), EventM, Next, Widget, AttrName, AttrMap,
    neverShowCursor, customMain, attrMap, hLimit, vBox, withBorderStyle, str, withAttr, 
    continue, halt, emptyWidget, padRight, padLeft, padTop, padAll, padBottom, hBox, attrName, (<+>)
  )


data Cell = Dark | Player | Jellyfish | Mine | LeftShark | RightShark | Empty

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- | Draw the UI of the game
drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawGrid g <+> drawStats g))]

drawStats :: Game -> Widget Name
drawStats g = hLimit 30 (vBox [drwaMode (g^.mode), padTop (Pad 2) (drawDepth (g^.depth)), padTop (Pad 2) (drawBestDepth (g^.maxDepth)),
                         padTop (Pad 2) (drawGameOver (g^.alive))])

drwaMode :: Mode -> Widget n
drwaMode m = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Current Mode ")
  $ C.hCenter
  $ padAll 1
  $ str (show m)

drawDepth :: Depth -> Widget Name
drawDepth n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Current Depth ")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

drawBestDepth :: Depth -> Widget Name
drawBestDepth n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Max Depth ")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

drawGameOver :: Bool -> Widget Name
drawGameOver False = withAttr gameOverAttr $ C.hCenter $ str "Game Over"
drawGameOver _    = emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeRounded 
  $ B.borderWithLabel (str " Diving ")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | inDark c (g^.player) (g^.depth) = Dark
      | c `elem`(g^.player)              = Player
      | inJellyFish c (g^.obstacles)     = Jellyfish
      | inMine c (g^.obstacles)          = Mine
      | inLeftShark c (g^.obstacles)     = LeftShark
      | inRightShark c (g^.obstacles)    = RightShark
      | otherwise                        = Empty

-- customMain initialVty buildVty mUserChan app initialAppState 
gameInit :: IO Game
gameInit = 
  do
    channel <- BChan.newBChan 10
    forkIO $ forever $ do
      BChan.writeBChan channel Tick
      threadDelay 400000
    state <- initState 0
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    customMain initialVty builder (Just channel) app state

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"
darkAttr :: AttrName 
darkAttr = attrName "darkAttr"
playerAttr :: AttrName
playerAttr = attrName "playerAttr"
jellyfishAttr :: AttrName
jellyfishAttr = attrName "jellyfishAttr"
leftSharkAttr :: AttrName
leftSharkAttr = attrName "leftSharkAttr"
rightSharkAttr :: AttrName 
rightSharkAttr = attrName "rightSharkAttr"
mineAttr :: AttrName
mineAttr = attrName "mineAttr"
emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

-- draw the grid
drawCell :: Cell -> Widget Name
drawCell Dark = withAttr darkAttr dark
drawCell Player    = withAttr playerAttr space
drawCell Empty   = withAttr emptyAttr space
drawCell Jellyfish = withAttr jellyfishAttr upArrow
drawCell Mine = withAttr mineAttr star
drawCell LeftShark = withAttr leftSharkAttr leftArrow
drawCell RightShark = withAttr rightSharkAttr rightArrow

-- shapes
dark :: Widget Name 
dark = str "~"
space :: Widget Name
space = str " "
star :: Widget Name
star = str "*"
upArrow :: Widget Name
upArrow = str "^"
leftArrow :: Widget Name
leftArrow = str "<"
rightArrow :: Widget Name
rightArrow = str ">"

-- color
theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (darkAttr, fg V.white `V.withStyle` V.bold)
 , (playerAttr, V.white `on` V.white)
 , (jellyfishAttr, fg V.yellow `V.withStyle` V.bold)
 , (mineAttr, fg V.red `V.withStyle` V.bold)
 , (leftSharkAttr, fg V.blue `V.withStyle` V.bold)
 , (rightSharkAttr, fg V.blue `V.withStyle` V.bold)
 , (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]