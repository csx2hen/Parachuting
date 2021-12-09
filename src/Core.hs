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
import Data.List (findIndex)
import GHC.IO.Handle.Types (Handle__(haDecoder))

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
-- every mode has a Modes(like frequency)
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
-- last depth of every kind of obstacle
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
-- If we want to change the difficulty, we could change the randomRs(x, y). Just Consider them as the cycle time of obstacle generator
modeMaps :: IO ModeMap
modeMaps = do
  x    <- randomRs (0, gridWidth) <$> newStdGen
  y    <- randomRs (0, last initPlayer^._2) <$> newStdGen
  easyShark <- randomRs (0, 10) <$> newStdGen
  easyMine <- randomRs (5, 10) <$> newStdGen
  easyJellyFish <- randomRs (5, 10) <$> newStdGen
  medium <- randomRs (3, 5) <$> newStdGen
  hard <- randomRs (0, 3) <$> newStdGen
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

-- | Set game's relevant mode.
setModes :: Modes -> Game -> Game
setModes m g = case g^.mode of
                  Easy -> g & modeMap.easy .~ m
                  Medium -> g & modeMap.medium .~ m
                  Hard -> g & modeMap.hard .~ m

-- change mode when we dive to the depth
modesOfDepth :: [Int]
modesOfDepth = [500, 100, 0]

-- modes we have now
modesType :: [Mode]
modesType = [Hard, Medium, Easy]

-- find which mode we shold use at current depth
findModes :: Depth -> Maybe Int
findModes d = findIndex (d >=) modesOfDepth

-- | change game's relevant mode.
changeModes :: Game -> Game
changeModes g = case findModes (g^.depth) of
                  Just x -> g & mode .~ (modesType !! x)
                  Nothing -> g

gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 20

-- InitLastDepth Setting
lastDepth :: LastDepth
lastDepth = LastDepth (-5) (-5) (-5) (-5)

-- InitPlayer Setting
initPlayer :: Player
initPlayer = [V2 (gridWidth `div` 2) (gridHeight - 3), V2 (gridWidth `div` 2) (gridHeight - 4)]

-- Init State 
initState :: Depth -> IO Game
initState maxDepth =
  do
    mode <- modeMaps
    return Game {
                  _player           = initPlayer,
                  _direction        = Still,
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


-- | Checks to see if the passed-in coordinate is in any Dark
inDark :: Coordinate -> Player -> Depth -> Bool
inDark c p d = (last p^._1 - c^._1) ^ 2 + (last p^._2 - c^._2) ^ 2 > max (800 - d) 50

-- | Checks to see if the passed-in coordinate is in any JellyFish
inJellyFish :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inJellyFish c bs = getAny $ foldMap (Any . inJellyFish' c) bs

-- | Checks to see if the passed-in coordinate is in a specific JellyFish
inJellyFish' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inJellyFish' c (b, Jellyfish) = c `elem` b
inJellyFish' _ _ = False

-- | Checks to see if the passed-in coordinate is in any Mine
inMine :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inMine c bs = getAny $ foldMap (Any . inMine' c) bs

-- | Checks to see if the passed-in coordinate is in a specific Mine
inMine' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inMine' c (b, Mine) = c `elem` b
inMine' _ _ = False

-- | Checks to see if the passed-in coordinate is in any LeftShark
inLeftShark :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inLeftShark c bs = getAny $ foldMap (Any . inLeftShark' c) bs

-- | Checks to see if the passed-in coordinate is in a specific LeftShark
inLeftShark' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inLeftShark' c (b, LeftShark) = c `elem` b
inLeftShark' _ _ = False

-- | Checks to see if the passed-in coordinate is in any RightShark
inRightShark :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
inRightShark c bs = getAny $ foldMap (Any . inRightShark' c) bs

-- | Checks to see if the passed-in coordinate is in a specific RightShark
inRightShark' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
inRightShark' c (b, RightShark) = c `elem` b
inRightShark' _ _ = False

-- Step Functions
-- | Step forward in time.
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ g^.alive && not (g^.paused)
  return $ fromMaybe (step' g) (checkAlive g)

-- | What to do if we are not dead.
step':: Game -> Game
step' = changeModes . recordMaxDepth . createObstacles . move . deleteObstaclesLeft . deleteObstaclesRight

-- Moving functions
-- | Move everything on the screen
move :: Game -> Game
move = movePlayer . moveObstacles

-- change direction of player(Up, Down)
changeDir :: Direction -> Game -> Game
changeDir d g = if g^.direction == Down && d == Down
                then g & movePlayer
                else g & direction .~ d

-- move player (Up, Down, Still). Consider up down and still as continuous state
movePlayer :: Game -> Game
movePlayer g = case g^.direction of
                Still -> g
                Up -> if shouldUp g then decDepth (moveObstaclesDir Down g) else changeDir Still g
                Down -> incDepth (moveObstaclesDir Up g)

-- Moves player handler (Left, Right). Consider left right as a movement
-- fromMaybe g (checkAlive (movePlayerHorizontally Left g))
movePlayerSingleStep :: Movement -> Game -> Game
movePlayerSingleStep Left g  = if shouldLeft g && g^.alive then movePlayerHorizontally Left $ fromMaybe g $ do return $ fromMaybe g (checkAlive g) else g
movePlayerSingleStep Right g = if shouldRight g && g^.alive then movePlayerHorizontally Right $ fromMaybe g $ do return $ fromMaybe g (checkAlive g) else g

-- | Moves player (Left, Right)
movePlayerHorizontally :: Movement -> Game -> Game
movePlayerHorizontally dir g =
  case dir of
    Left  -> if shouldLeft g then g & player %~ fmap (+ V2 (-1) 0) else g
    Right -> if shouldRight g then g & player %~ fmap (+ V2 1 0) else g

-- check if the palyer should go up
shouldUp :: Game -> Bool
shouldUp g = shouldUp' $ g^.depth

shouldUp' :: Depth -> Bool
shouldUp' d = d > 0

-- check if the palyer should go left
shouldLeft :: Game -> Bool
shouldLeft g = shouldLeft' [coord^._1 | coord <- g^.player]

shouldLeft' :: [Int] -> Bool
shouldLeft' xs = (xs /= []) && (minimum xs) > 0

-- check if the palyer should go right
shouldRight :: Game -> Bool
shouldRight g = shouldRight' [coord^._1 | coord <- g^.player]

shouldRight' :: [Int] -> Bool
shouldRight' xs = (xs /= []) && (minimum xs) < gridWidth - 1

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

-- | Move single obstacle towards the direction given
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

-- increase depth
incDepth :: Game -> Game
incDepth g = g & depth %~ (+1)

-- decrease depth
decDepth :: Game -> Game
decDepth g = g & depth %~ (+(-1))

-- record max depth
recordMaxDepth :: Game -> Game
recordMaxDepth g = if g^.depth > g^.maxDepth then g & maxDepth .~ (g^.depth) else g

-- check to see if alive
checkAlive :: Game -> Maybe Game
checkAlive g = do
  guard $ isDead g
  return $ g & alive .~ False

-- check to see if dead
isDead :: Game -> Bool
isDead g = let player' = g^.player
               obstacles' = g^.obstacles
              in getAny $ foldMap (Any . flip crash obstacles') player'

-- check to see if crashed in any obstacle
crash :: Coordinate -> SEQ.Seq (Obstacle, ObstacleType) -> Bool
crash player obstacles = getAny $ foldMap (Any . crash' player) obstacles

-- check to see if crashed in a specific obstacle
crash' :: Coordinate -> (Obstacle, ObstacleType) -> Bool
crash' player obstacle = player `elem` fst obstacle

-- create three types of obstacles(jellyfish, mine, shark)
createObstacles :: Game -> Game
createObstacles g = addRandomObstacle RightShark $ addRandomObstacle LeftShark $ addRandomObstacle Mine $ addRandomObstacle Jellyfish g

-- create obstacle of given type 
addRandomObstacle :: ObstacleType -> Game -> Game
addRandomObstacle Jellyfish g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle Jellyfish x
                                    in
                                      if g^.depth - g^.lastObstaleDepth.jellyfish >= j
                                      then setModes newModes g & obstacles %~ (|> newObs) & ((lastObstaleDepth.jellyfish) .~ g^.depth)
                                      else g
addRandomObstacle Mine      g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle Mine x
                                    in
                                      if g^.depth - g^.lastObstaleDepth.mine >= m
                                      then setModes newModes g & obstacles %~ (|> newObs) & ((lastObstaleDepth.mine) .~ g^.depth)
                                      else g
addRandomObstacle LeftShark g =   let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle LeftShark y
                                    in
                                      if g^.depth - g^.lastObstaleDepth.leftShark >= l
                                      then setModes newModes g & obstacles %~ (|> newObs) & ((lastObstaleDepth.leftShark) .~ g^.depth)
                                      else g
addRandomObstacle RightShark g =  let (Modes (x:xs) (y:ys) (j:js) (m:ms) (l:ls) (r:rs)) = getModes g
                                      newModes = Modes xs ys js ms ls rs
                                      newObs = createObstacle RightShark y
                                    in
                                      if g^.depth - g^.lastObstaleDepth.rightShark >= r
                                      then setModes newModes g & obstacles %~ (|> newObs) & ((lastObstaleDepth.rightShark) .~ g^.depth)
                                      else g


-- | Make a obstacle. The width and height are determined by
-- the dimension, and the second parameter determines the
-- starting ypos.
createObstacle :: ObstacleType -> Int -> (Obstacle, ObstacleType)
createObstacle obsType pos = (getObstacleCoord obsType pos, obsType)

-- get coords of given obstacle
getObstacleCoord :: ObstacleType -> Int -> Obstacle
getObstacleCoord Mine       x = [V2 x 0, V2 x (-1), V2 x (-2), V2 (x - 1) (-1), V2 (x + 1) (-1)]
getObstacleCoord RightShark y = [V2 0 y, V2 1 y, V2 2 y]
getObstacleCoord LeftShark  y = [V2 gridWidth y, V2 (gridWidth - 1) y, V2 (gridWidth - 2) y]
getObstacleCoord Jellyfish  x = [V2 x 0, V2 x (-1), V2 (x - 1) 0, V2 (x + 1) 0]

-- | Delete obstacle
deleteObstaclesLeft :: Game -> Game
deleteObstaclesLeft g = case viewl $ g^.obstacles of
                      EmptyL  -> g
                      a :< as -> if isOutOfBoundary a
                                  then deleteObstaclesLeft (g & obstacles .~ as)
                                  else g
-- | Delete obstacle
deleteObstaclesRight :: Game -> Game
deleteObstaclesRight g = case viewr $ g^.obstacles of
                      EmptyR  -> g
                      as :> a -> if isOutOfBoundary a
                                  then deleteObstaclesRight (g & obstacles .~ as)
                                  else g
-- | check the obastacle is out of boundray
isOutOfBoundary :: (Obstacle, ObstacleType) -> Bool
isOutOfBoundary (coords, Mine)       = (coords !! 2)^._2 > gridHeight
isOutOfBoundary (coords, RightShark) = head coords^._1 > gridWidth
isOutOfBoundary (coords, LeftShark)  = head coords^._1 < 0
isOutOfBoundary (coords, Jellyfish)  = last coords^._2 > gridHeight
