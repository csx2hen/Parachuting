{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Core where

import Data.Monoid

import System.Random (Random(..), randomRs, newStdGen)
import Linear.V2 (V2(..))
import qualified Data.Sequence as SEQ
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Prelude hiding (Right, Left)
import Data.Sequence (ViewR(EmptyR, (:>)), viewr, (|>), ViewL (EmptyL, (:<)), viewl)
import Graphics.Vty.PictureToSpans (isOutOfBounds)

type Name = ()
type Depth = Int
type Coordinate = V2 Int
type Player = [Coordinate] -- list of Coordinates
type Obstacle = [Coordinate] -- list of Coordinates
data ObstacleType = Jellyfish | Mine | LeftShark | RightShark deriving (Eq, Show)
data Tick = Tick
data Direction = Up | Down | Still deriving (Eq, Show)
data Movement = Left | Right
data Mode = Easy | Medium | Hard deriving (Eq, Show)
data ModeMap = ModeMap
  {
    _easy   :: Modes,
    _medium :: Modes,
    _hard   :: Modes
  }
  deriving (Eq, Show)
type Frequency = [Int]
data Modes = Modes
  {
    _x                   :: [Int],
    _y                   :: [Int],
    _jellyfishFrequency  :: Frequency,
    _mineFrequency       :: Frequency,
    _leftSharkFrequency  :: Frequency,
    _rightSharkFrequency :: Frequency
  }
  deriving (Eq, Show)
data LastDepth = LastDepth
  {
    _jellyfish  :: Depth,
    _mine       :: Depth,
    _leftShark  :: Depth,
    _rightShark :: Depth
  }
  deriving (Eq, Show)
data Game = Game
  {
    _player           :: Player,
    _direction        :: Direction,
    _obstacles        :: SEQ.Seq (Obstacle, ObstacleType),
    _depth            :: Depth,
    _initDepth        :: Depth,
    _maxDepth         :: Depth,
    _alive            :: Bool,
    _paused           :: Bool,
    _modeMap          :: ModeMap,
    _mode             :: Mode,
    _lastObstaleDepth :: LastDepth
  } deriving (Show)

makeLenses ''Game
makeLenses ''ModeMap
makeLenses ''LastDepth
makeLenses ''Modes

-- Mode Setting
modeMaps :: IO ModeMap
modeMaps = do
  x    <- randomRs (0, gridWidth) <$> newStdGen
  y    <- randomRs (0, last initPlayer^._2 - 2) <$> newStdGen
  easyShark <- randomRs (5, 10) <$> newStdGen
  easyMine <- randomRs (5, 10) <$> newStdGen
  easyJellyFish <- randomRs (5, 10) <$> newStdGen
  medium <- randomRs (3, 5) <$> newStdGen
  hard <- randomRs (1, 3) <$> newStdGen
  return $ ModeMap
    (Modes x y easyJellyFish easyMine easyShark easyShark)
    (Modes x y medium medium medium medium)
    (Modes x y hard hard hard hard)

-- | Get game's relevant mode.
getModes :: Game -> Modes
getModes g = case g^.mode of
                Easy -> g^.modeMap.easy
                Medium -> g^.modeMap.medium
                Hard -> g^.modeMap.hard

setModes :: Modes -> Game -> Game
setModes m g = case g^.mode of
                  Easy -> g & modeMap.easy .~ m
                  Medium -> g & modeMap.medium .~ m
                  Hard -> g & modeMap.hard .~ m


-- Constants
gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 20

-- InitLastDepth Setting
lastDepth :: LastDepth
lastDepth = LastDepth (-5) (-5) (-5) (-5)

initPlayer :: Player
initPlayer = [V2 (gridWidth `div` 2) (gridHeight - 3), V2 (gridWidth `div` 2) (gridHeight - 4)]

initState :: Depth -> IO Game
initState maxDepth =
  do
    mode <- modeMaps
    return Game {
                  _player           = initPlayer,
                  _direction        = Still,
                  -- _obstacles        = SEQ.fromList [createObstacle Jellyfish 0, createObstacle Mine 3, createObstacle LeftShark 10, createObstacle RightShark 15],
                  _obstacles        = SEQ.empty,
                  _depth            = 0,
                  _maxDepth         = maxDepth,
                  _alive            = True,
                  _paused           = False,
                  _initDepth        = 0,
                  _modeMap          = mode,
                  _mode             = Easy,
                  _lastObstaleDepth = lastDepth
                }


-- | Checks to see if the passed-in coordinate is in any
-- of the barriers
inBarriers :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inBarriers c bs = getAny $ foldMap (Any . inBarrier c) bs

-- | Checks to see if the passed-in coordinate is in the
-- passed-in barriers
inBarrier :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inBarrier c (b, Jellyfish) = c `elem` b
inBarrier _ _ = False

inBarriersMine :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inBarriersMine c bs = getAny $ foldMap (Any . inBarrierMine' c) bs

inBarriersLeftShark :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inBarriersLeftShark c bs = getAny $ foldMap (Any . inBarriersLeftShark' c) bs

inBarriersRightShark :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inBarriersRightShark c bs = getAny $ foldMap (Any . inBarriersRightShark' c) bs

-- | Checks to see if the passed-in coordinate is in the
-- passed-in barriers
inBarrierMine' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inBarrierMine' c (b, Mine) = c `elem` b
inBarrierMine' _ _ = False

inBarriersLeftShark' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inBarriersLeftShark' c (b, LeftShark) = c `elem` b
inBarriersLeftShark' _ _ = False

inBarriersRightShark' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inBarriersRightShark' c (b, RightShark) = c `elem` b
inBarriersRightShark' _ _ = False

-- Functions
-- | Step forward in time.
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ g^.alive && not (g^.paused)
  return $ fromMaybe (step' g) (checkAlive g)

-- | What to do if we are not dead.
step':: Game -> Game
step' = recordMaxDepth . createObstacles . move . deleteObstaclesLeft . deleteObstaclesRight
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
move = movePlayer . moveObstacles

-- change direction of player(Up, Down)
changeDir :: Direction -> Game -> Game
changeDir d g = if g^.direction == Down && d == Down
                then g & movePlayer
                else g & direction .~ d

-- move player (Up, Down, Still)
movePlayer :: Game -> Game
movePlayer g = case g^.direction of
                Still -> g
                Up -> if shouldUp g then decDepth (moveObstaclesDir Down g) else changeDir Still g
                Down -> incDepth (moveObstaclesDir Up g)

-- | Moves player (Left, Right)
movePlayerHorizontally :: Movement -> Game -> Game
movePlayerHorizontally dir g =
  case dir of
    Left  -> if shouldLeft g then g & player %~ fmap (+ V2 (-1) 0) else g
    Right -> if shouldRight g then g & player %~ fmap (+ V2 1 0) else g

shouldUp :: Game -> Bool
shouldUp g = g^.depth > 0

shouldLeft :: Game -> Bool
shouldLeft g = (minimum [coord^._1 | coord <- g^.player]) > 0

shouldRight :: Game -> Bool
shouldRight g = (minimum [coord^._1 | coord <- g^.player]) < gridWidth - 1

movePlayerSingleStep :: Movement -> Game -> Game
movePlayerSingleStep Left g  = if shouldLeft g && g^.alive then movePlayerHorizontally Left g else g
movePlayerSingleStep Right g = if shouldRight g && g^.alive then movePlayerHorizontally Right g else g

-- Obstacle functions
-- | Move all the obstacles
moveObstacles :: Game -> Game
moveObstacles g = g & obstacles %~ fmap moveObstacle

-- | Move single obstacle(Jellyfish) Up, obstacle(LeftShark) left, obstacle(RightShark) right
moveObstacle :: (Obstacle, ObstacleType) -> (Obstacle, ObstacleType)
moveObstacle (obs, Jellyfish)   = (fmap (+ V2 0 1) obs, Jellyfish)
moveObstacle (obs, LeftShark)   = (fmap (+ V2 (-1) 0) obs, LeftShark)
moveObstacle (obs, RightShark)  = (fmap (+ V2 1 0) obs, RightShark)
moveObstacle other              = other

-- | Move single obstacle
moveObstaclesDir :: Direction -> Game -> Game
moveObstaclesDir Up    g = g & obstacles %~ fmap moveObstacleUp
moveObstaclesDir Down  g = g & obstacles %~ fmap moveObstacleDown
moveObstaclesDir _     g = g

-- | Move single obstacle Up
moveObstacleUp :: (Obstacle, ObstacleType) -> (Obstacle, ObstacleType)
moveObstacleUp (obs, ty) = (fmap (+ V2 0 1) obs, ty)

-- | Move single obstacle Down
moveObstacleDown :: (Obstacle, ObstacleType) -> (Obstacle, ObstacleType)
moveObstacleDown (obs, ty) = (fmap (+ V2 0 (-1)) obs, ty)

incDepth :: Game -> Game
incDepth g = g & depth %~ (+1)

decDepth :: Game -> Game
decDepth g = g & depth %~ (+(-1))

recordMaxDepth :: Game -> Game
recordMaxDepth g = if g^.depth > g^.maxDepth then g & maxDepth .~ (g^.depth) else g

checkAlive :: Game -> Maybe Game
checkAlive g = do
  guard $ isDead g
  return $ g & alive .~ False

isDead :: Game -> Bool
isDead g = let player' = g^.player
               obstacles' = g^.obstacles
              in getAny $ foldMap (Any . flip crash obstacles') player'

crash :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
crash player obstacles = getAny $ foldMap (Any . crash' player) obstacles

crash' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
crash' player obstacle = player `elem` fst obstacle

-- create three types of obstacles(jellyfish, mine, shark)
createObstacles :: Game -> Game
createObstacles g = addRandomObstacle RightShark $ addRandomObstacle LeftShark $ addRandomObstacle Mine $ addRandomObstacle Jellyfish g

addRandomObstacle :: ObstacleType -> Game -> Game
addRandomObstacle Jellyfish g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle Jellyfish x
                                  in
                                    if g^.depth - g^.lastObstaleDepth.jellyfish >= head (getModes g^.jellyfishFrequency)
                                    then setModes newModes g & obstacles %~ (|> newObs) & (lastObstaleDepth.jellyfish) .~ g^.depth
                                    else g
addRandomObstacle Mine      g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle Mine x
                                  in
                                    if g^.depth - g^.lastObstaleDepth.mine >= head (getModes g^.mineFrequency)
                                    then setModes newModes g & obstacles %~ (|> newObs) & (lastObstaleDepth.mine) .~ g^.depth
                                    else g
addRandomObstacle LeftShark g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle LeftShark y
                                  in
                                    if g^.depth - g^.lastObstaleDepth.leftShark >= head (getModes g^.leftSharkFrequency)
                                    then setModes newModes g & obstacles %~ (|> newObs) & (lastObstaleDepth.leftShark) .~ g^.depth
                                    else g
addRandomObstacle RightShark g =  let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle RightShark y
                                  in
                                    if g^.depth - g^.lastObstaleDepth.rightShark >= head (getModes g^.rightSharkFrequency)
                                    then setModes newModes g & obstacles %~ (|> newObs) & (lastObstaleDepth.rightShark) .~ g^.depth
                                    else g


-- | Make a obstacle. The width and height are determined by
-- the dimension, and the second parameter determines the
-- starting ypos.
createObstacle :: ObstacleType -> Int -> (Obstacle, ObstacleType)
createObstacle obsType pos = (getObstacleCoord obsType pos, obsType)

getObstacleCoord :: ObstacleType -> Int -> Obstacle
getObstacleCoord Mine       x = [V2 x 0, V2 x (-1), V2 x (-2), V2 (x - 1) (-1), V2 (x + 1) (-1)]
getObstacleCoord RightShark y = [V2 0 y, V2 1 y, V2 2 y]
getObstacleCoord LeftShark  y = [V2 gridWidth y, V2 (gridWidth - 1) y, V2 (gridWidth - 2) y]
getObstacleCoord Jellyfish  x = [V2 x 0, V2 x (-1)]

-- | Delete barrier if it has gone off the left side
deleteObstaclesLeft :: Game -> Game
deleteObstaclesLeft g = case viewl $ g^.obstacles of
                      EmptyL  -> g
                      a :< as -> if isOutOfBoundary a 
                                  then deleteObstaclesLeft (g & obstacles .~ as)
                                  else g
deleteObstaclesRight :: Game -> Game
deleteObstaclesRight g = case viewr $ g^.obstacles of
                      EmptyR  -> g
                      as :> a -> if isOutOfBoundary a 
                                  then deleteObstaclesRight (g & obstacles .~ as)
                                  else g

isOutOfBoundary :: (Obstacle, ObstacleType) -> Bool
isOutOfBoundary (coords, Mine)       = (coords !! 2)^._2 > gridHeight
isOutOfBoundary (coords, RightShark) = head coords^._1 > gridWidth
isOutOfBoundary (coords, LeftShark)  = head coords^._1 < 0
isOutOfBoundary (coords, Jellyfish)  = last coords^._2 > gridHeight