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
import Data.Sequence (ViewR(EmptyR, (:>)), viewr)

type Name = ()
type Depth = Int
type Coordinate = V2 Int
type Player = [Coordinate] -- list of Coordinates
type Obstacle = [Coordinate] -- list of Obstacles
data ObstacleType = Jellyfish | Mine | LeftShark | RightShark deriving (Eq, Show)

data Tick = Tick

data Direction = Up | Down | Still deriving (Eq, Show)
data Movement = Left | Right

data Game = Game
  {
    _player    :: Player,
    _direction :: Direction,
    _obstacles :: SEQ.Seq (Obstacle, ObstacleType),
    _depth     :: Depth,
    _initDepth :: Depth,
    _maxDepth  :: Depth,
    _alive     :: Bool,
    _paused    :: Bool
  } deriving (Show)

makeLenses ''Game

-- Constants
gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 20


initPlayer :: Player
initPlayer = [V2 (gridWidth `div` 2) (gridHeight - 3), V2 (gridWidth `div` 2) (gridHeight - 4)]

initState :: Depth -> IO Game
initState maxDepth =
  do
    return Game {
                  _player    = initPlayer,
                  _direction = Still,
                  _obstacles = SEQ.fromList [([V2 0 1, V2 0 2], Jellyfish), ([V2 2 2, V2 3 2, V2 4 2, V2 3 1, V2 3 3], Mine), ([V2 (gridWidth-2) 4, V2 (gridWidth-1) 4, V2 gridWidth 4], LeftShark), ([V2 0 5, V2 1 5, V2 2 5], RightShark)],
                  _depth     = 0,
                  _maxDepth  = maxDepth,
                  _alive     = True,
                  _paused    = False,
                  _initDepth  = 0
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


--TODO delete obstacles and create obstacles
-- | What to do if we are not dead.
step':: Game -> Game
step' = recordMaxDepth . createObstacles . move 
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
changeDir d g = g & direction .~ d

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
createObstacles g = g
-- createObstacles :: Game -> Game
-- createObstacles g = 
--   case viewr $ g^.obstacles of
--     EmptyR -> addRandomObstacle g
--     _ :> a -> addRandomObstacle g

-- addRandomObstacle g = 
