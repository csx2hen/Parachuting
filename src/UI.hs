module UI where

import Core
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.BChan as BChan
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Lens.Micro ((^.), mapped)
import Linear.V2 (V2(..))
import Core (Game(Game))
import Control.Monad ( forever )
import Brick.Util ( fg, on )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import GHC.IO (unsafePerformIO)
import Controller(handleEvent, handleEventStep)

import Brick
  ( App(..), BrickEvent(..), Padding(..), EventM, Next, Widget, AttrName, AttrMap,
    neverShowCursor, customMain, attrMap, hLimit, vBox, withBorderStyle, str, withAttr, 
    continue, halt, emptyWidget, padRight, padLeft, padTop, padAll, padBottom, hBox, attrName, (<+>)
  )


data Cell = Player | Obstacle | Empty

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEventStep
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- | Draw the UI of the game
drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawGrid g <+> drawStats g))]

drawStats :: Game -> Widget Name
drawStats g = hLimit 30 (vBox [drawScore (g^.score), padTop (Pad 2) (drawBestScore (g^.highScore)),
                         padTop (Pad 2) (drawGameOver (g^.alive))])

drawScore :: Score -> Widget Name
drawScore n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " current score ")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

drawBestScore :: Score -> Widget Name
drawBestScore n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " best score ")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

drawGameOver :: Bool -> Widget Name
drawGameOver True = withAttr gameOverAttr $ C.hCenter $ str "game over"
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
      | c `elem`(g^.player)        = Player
      | inBarriers c (g^.obstacles) = Obstacle
      | otherwise                  = Empty

counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

-- customMain initialVty buildVty mUserChan app initialAppState 
gameInit :: IO Game
gameInit = 
  do
    channel <- BChan.newBChan 10
    forkIO $ forever $ do
      modifyIORef counter (+1)
      c' <- readIORef counter
      BChan.writeBChan channel Tick
      threadDelay (max (65000 - c' * 10) 250000)
    -- threadDelay 35000
    state <- initState 0
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    customMain initialVty builder (Just channel) app state

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

playerAttr :: AttrName
playerAttr = attrName "playerAttr"

obstacleAttr :: AttrName
obstacleAttr = attrName "obstacleAttr"

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"


drawCell :: Cell -> Widget Name
drawCell Player    = withAttr playerAttr cw
drawCell Obstacle = withAttr obstacleAttr cw
drawCell Empty   = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "




theMap :: AttrMap
theMap = attrMap V.defAttr
 [ (playerAttr, V.white `on` V.white)
 , (obstacleAttr, V.red `on` V.red)
 , (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]