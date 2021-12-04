{-# LANGUAGE TemplateHaskell #-}
module Core where

import Data.Monoid

import System.Random (Random(..), randomRs, newStdGen)
import Linear.V2 (V2(..))
import qualified Data.Sequence as SEQ
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Prelude hiding (Right, Left)

type Name = ()
type Score = Int
type Coordinate = V2 Int
type Player = [Coordinate] -- list of Coordinates
type Obstacle = [Coordinate] -- list of Obstacles

data Tick = Tick

data Direction = Left| Right | Up | Down | Still deriving (Eq, Show)

data Game = Game
  { 
    _player    :: Player,
    _direction :: Direction,
    _obstacles :: SEQ.Seq Obstacle,
    _score     :: Score,
    _initScore :: Score,
    _highScore :: Score,
    _alive     :: Bool,
    _paused    :: Bool
  } deriving (Show)


makeLenses ''Game



-- Constants
gridWidth :: Int
gridWidth = 30
gridHeight :: Int
gridHeight = 40


initPlayer :: Player
initPlayer = [V2 (gridWidth `div` 2) (gridHeight - 1), V2 (gridWidth `div` 2) (gridHeight - 2)]

initState :: Score -> IO Game
initState highestScore = 
  do
    return Game {
                  _player    = initPlayer,
                  _direction = Still,
                  _obstacles = SEQ.fromList [[V2 0 1, V2 0 2]],
                  _score     = 0,
                  _highScore = highestScore,
                  _alive     = True,
                  _paused    = False,
                  _initScore  = 0
                }


-- | Checks to see if the passed-in coordinate is in any
-- of the barriers
inBarriers :: Coordinate -> SEQ.Seq Obstacle -> Bool
inBarriers c bs = getAny $ foldMap (Any . inBarrier c) bs

-- | Checks to see if the passed-in coordinate is in the
-- passed-in barriers
inBarrier :: Coordinate -> Obstacle -> Bool
inBarrier c b = c `elem` b

-- Functions
-- | Step forward in time.
-- Increment score every tick.
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ g^.alive && not (g^.paused)
  return $ step' g
-- $ fromMaybe (stepHelper g) -- (die g)

--TODO delete obstacles and create obstacles
-- | What to do if we are not dead.
step':: Game -> Game
step' = move
  {- incDifficulty . setHighScore . incScore . move . spawnBarrier .
          deleteBarrier . adjustStanding . adjustDuckCountdown -}


-- | Possibly die if next position is disallowed.
-- die :: Game -> Maybe Game
-- die g = do
--   guard $ die' g
--   return $ g & dead .~ True


-- Moving functions
-- | Move everything on the screen
-- if we want to use Single step mode, we need to delete movePlayer
move :: Game -> Game
move ={-  movePlayer . -} moveObstacles . createObstacles . incScore


movePlayer :: Game -> Game
movePlayer g = let dir = g^.direction in
  case dir of
    Down   -> movePlayerVertically Down g 
    Up     -> movePlayerVertically Up g 
    Left   -> movePlayerHorizontally Left g
    Right  -> movePlayerHorizontally Right g
    Still  -> g
{-     Up   -> if shouldStopDino d g then setDinoDir Down g else moveDino' 1 g
    Down -> if shouldStopDino d g then setDinoDir Still g else
              (let gNext = moveDino' (-1) g in
                if isDinoBottom gNext then setDinoDir Still gNext else gNext)
    Duck -> if shouldStopDino d g then g else moveDino' (-1) g -}

-- | Moves player Up or Down
movePlayerVertically :: Direction -> Game -> Game
movePlayerVertically dir g = 
  case dir of
    Up -> if shouldUp g then g & player %~ fmap (+ V2 0 1) else g
    Down  -> if shouldDown g then g & player %~ fmap (+ V2 0 (-1)) else g
    _ -> g

-- | Moves player Left or Right
movePlayerHorizontally :: Direction -> Game -> Game
movePlayerHorizontally dir g = 
  case dir of
    Left  -> if shouldLeft g then g & player %~ fmap (+ V2 (-1) 0) else g
    Right -> if shouldRight g then g & player %~ fmap (+ V2 1 0) else g
    _ -> g

shouldUp :: Game -> Bool
shouldUp g = (maximum [coord^._2 | coord <- g^.player]) < gridHeight - 1

shouldDown :: Game -> Bool
shouldDown g = (minimum [coord^._2 | coord <- g^.player]) > 0

shouldLeft :: Game -> Bool
shouldLeft g = (minimum [coord^._1 | coord <- g^.player]) > 0

shouldRight :: Game -> Bool
shouldRight g = (minimum [coord^._1 | coord <- g^.player]) < gridWidth - 1

moveSingleStep :: Direction -> Game -> Game
moveSingleStep dir g = 
  case dir of
    Left -> if shouldLeft g then movePlayerHorizontally Left g else g
    Right -> if shouldLeft g then movePlayerHorizontally Right g else g
    Up -> if shouldUp g then movePlayerVertically Up g else g
    Down -> if shouldDown g then movePlayerVertically Down g else g
    Still -> g

-- Obstacle functions
-- | Move all the obstacles
moveObstacles :: Game -> Game
moveObstacles g = g & obstacles %~ fmap moveObstacle

-- | Move single obstacle Up
moveObstacle :: Obstacle -> Obstacle
moveObstacle = fmap (+ V2 0 1)

createObstacles :: Game -> Game
createObstacles g = g 


constScoreMod :: Score
constScoreMod = 3

incScore :: Game -> Game
incScore g = case g^.initScore of
  0 -> g & score %~ (+1) & initScore %~ incAndMod
  _ -> g & initScore %~ incAndMod
  where incAndMod x = (x + 1) `mod` constScoreMod