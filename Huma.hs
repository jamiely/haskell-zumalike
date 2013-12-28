module Huma where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics.Aliases(orElse)
import Control.Monad
import System.Random (StdGen)
import Data.List (find, elemIndex)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Width = Float
type BallId = Int
type Index = Int
type Tick = Int
data BallColor = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Eq, Show, Ord)

colors :: Seq BallColor
colors = Seq.fromList [Red, Green, Blue, Yellow, Cyan, Magenta]

colorMod :: Int -> BallColor
colorMod i = Seq.index colors $ i `rem` (Seq.length colors)

data Point = Point Float Float deriving (Eq, Show)
data IndexedPoint = IndexedPoint Index Point deriving (Eq, Show)

-- a regular ball in Zuma
data Ball = Ball BallId Width BallColor deriving (Eq, Ord, Show)
-- a contiguous segment which a ball may traverse
data Path = PointPath [IndexedPoint] deriving (Eq, Show)
-- A collection of paths that feed into each other
data Way = Way [Path] deriving (Eq, Show)
data Chain = Chain [Ball] deriving (Eq, Show)
data Position = Position Ball IndexedPoint deriving (Eq, Show)
data Positions = PositionMap (Map Ball Position) deriving (Eq, Show)
-- A Transit describes a chain on a certain "Way"
data Transit = Transit Chain Way Positions deriving (Eq, Show)

data BallGenerator = SequentialGenerator Index Width deriving (Show, Eq)
data GameState = GameState BallGenerator [Transit] deriving (Show, Eq)
type GameTick = (GameState, Tick)
-- list of game ticks, in order of descending time
data Game = Game [GameTick]

generateBall :: BallGenerator -> (Ball, BallGenerator)
generateBall (SequentialGenerator i w) = (ball, newGen) where
  ball = Ball i w $ colorMod i
  newGen = SequentialGenerator (i + 1) w

euclideanDistance :: Point -> Point -> Float
euclideanDistance (Point x1 y1) (Point x2 y2) = 
  sqrt $ (x1 - x2)**2 + (y1 - y2)**2

-- Returns the first way point availble on the way
firstWayPoint :: Way -> Maybe IndexedPoint
firstWayPoint (Way ((PointPath (pt:_)):_)) = Just pt
firstWayPoint _ = Nothing

defaultBallWidth :: Width
defaultBallWidth = 20

updateTransitPositions :: Transit -> Transit
updateTransitPositions (Transit c@(Chain balls) way originalPositions) =
  Transit c way newPositions where
  ballPairs = ballAndPreviousBall balls
  newPositions = foldl (flip $ updatePositionsUsingPrev way) originalPositions ballPairs

updateGameStatePositions :: GameState -> GameState
updateGameStatePositions (GameState gen transits) = GameState gen $ map updateTransitPositions transits

ballAndPreviousBall :: [Ball] -> [(Ball, Maybe Ball)]
ballAndPreviousBall [] = []
ballAndPreviousBall balls = zip balls $ Nothing : map Just balls

updatePositionsUsingPrev :: Way -> (Ball, Maybe Ball) -> Positions -> Positions
updatePositionsUsingPrev way (ball, Just previous) originalPositions = 
  case maybePositions of
    Just p -> p
    Nothing -> originalPositions
    where
      lookup = getPosition originalPositions

      maybePositions :: Maybe Positions
      maybePositions = do
        prevPos <- lookup previous
        ballPos <- lookup ball
        newPos <- if needsNewPosition ballPos prevPos
                    then nonCollidingPosition way prevPos ballPos
                    else Nothing
        return $ updatePosition newPos originalPositions

updatePositionsUsingPrev _ (_, Nothing) ps = ps

needsNewPosition :: Position -> Position -> Bool
needsNewPosition ballPos prevPos = collides || lteIndex where
  collides = isColliding ballPos prevPos 
  lteIndex = compareIndices (<=) ballPos prevPos

compareIndices :: (Index -> Index -> Bool) -> Position -> Position -> Bool
compareIndices fun (Position _ (IndexedPoint i1 _)) (Position _ (IndexedPoint i2 _)) = 
  fun i1 i2

updatePosition :: Position -> Positions -> Positions
updatePosition p@(Position ball point) (PositionMap map) = 
  PositionMap $ Map.insert ball p map 

getPosition :: Positions -> Ball -> Maybe Position
getPosition (PositionMap map) ball = Map.lookup ball map where 

isColliding :: Position -> Position -> Bool
isColliding (Position (Ball _ w1 _) (IndexedPoint _ pt1)) 
  (Position (Ball _ w2 _) (IndexedPoint _ pt2)) =
    (euclideanDistance pt1 pt2) < (w1 + w2)

-- Returns a new ball position updated along the way so it is not colliding
-- with the first ball position.
nonCollidingPosition :: Way -> Position -> Position -> Maybe Position
nonCollidingPosition (Way (path:paths)) noCollide pos = 
  case nonCollidingPositionAlongPath path noCollide pos of
    newPos@(Just _) -> newPos
    Nothing -> nonCollidingPosition (Way paths) noCollide pos
nonCollidingPosition (Way []) _ _ = Nothing

nonCollidingPositionAlongPath :: Path -> Position -> Position -> Maybe Position
nonCollidingPositionAlongPath (PointPath points) = nonCollidingPositionAlongPoints points

nonCollidingPositionAlongPoints :: [IndexedPoint] -> Position -> Position -> Maybe Position
nonCollidingPositionAlongPoints points noCollide@(Position _ nPt@(IndexedPoint nInd _)) (Position pBall _) = 
  maybePosition where 
    maybePosition = do
      point <- find (not . pred) possiblePts
      index <- elemIndex point points
      return $ Position pBall point
    possiblePts = case dropWhile (/= nPt) points of
                    [] -> []
                    l:ls -> ls
    collides :: IndexedPoint -> Bool
    collides pt = isColliding noCollide (Position pBall pt)
    pred :: IndexedPoint -> Bool
    pred pt@(IndexedPoint index _) = collides pt || nInd >= index

incrementPointIndex :: Way -> IndexedPoint -> Maybe IndexedPoint
incrementPointIndex way (IndexedPoint index pt) = wayPointGivenIndex way $ index + 1

wayPointGivenIndex :: Way -> Index -> Maybe IndexedPoint
wayPointGivenIndex (Way ((PointPath points):ps)) i = if (length points) < i 
                                                       then Just (points !! i)
                                                       else Nothing
wayPointGivenIndex _ _ = Nothing

newBall :: GameState -> (Ball, GameState)
newBall (GameState gen transits) = (ball, game) where 
  (ball, newGen) = generateBall gen
  game = GameState newGen transits

-- Construction functions

emptyGameState :: GameState
emptyGameState = GameState (SequentialGenerator 1 defaultBallWidth) []

addTransitToGameState :: GameState -> Transit -> GameState
addTransitToGameState (GameState gen transits) t = GameState gen (t:transits)

emptyTransit :: Way -> Transit
emptyTransit way = Transit (Chain []) way $ PositionMap Map.empty

-- Adds a ball to the passed transit structure, setting its position to the
-- first waypoint in the way. The transit should be updated to adjust all
-- positions as soon as possible using `updateTransitPositions`
addBallToTransit :: Ball -> Transit -> Transit
addBallToTransit ball (Transit (Chain balls) way positions) = 
  Transit newChain way newPositions where
    newChain = Chain $ ball:balls
    newPositions = case maybeNewPosition of 
                     Just newPosition -> updatePosition newPosition positions
                     Nothing -> positions
    maybeNewPosition = liftM (Position ball) $ firstWayPoint way

addBallToGameStateInTransit :: GameState -> Transit -> Ball -> (GameState, Maybe Transit)
addBallToGameStateInTransit (GameState gen transits) transit ball = (game, maybeNewTransit) where
  game = GameState gen newTransits 
  newTransitPairs = map update transits
  newTransits = map fst newTransitPairs 
  maybeNewTransit = liftM fst $ find snd newTransitPairs 
  -- updates the transit if it matches the `way` we are looking for
  update t = if transit == t
               then (addBallToTransit ball t, True)
               else (t, False) 

addNewBallToGameStateInTransit :: GameState -> Transit -> (GameState, Maybe Transit)
addNewBallToGameStateInTransit gameState transit = finalGameState where
  (ball, newGameState) = newBall gameState
  finalGameState = addBallToGameStateInTransit newGameState transit ball

-- Setup some fake data to render

fakeBalls :: [Ball]
fakeBalls = [Ball i defaultBallWidth (colorMod i) | i <- [1 .. 3]]

fakeWay :: Way 
fakeWay = Way [PointPath points] where
  points = map (\(x, y) -> IndexedPoint x y) 
    [(x, Point (fromIntegral x) 10) | x <- [0, 5 .. 30]]

fakeTransit :: Transit
fakeTransit = emptyTransit fakeWay

fakeGameState :: GameState
fakeGameState = game where
  transit = fakeTransit
  b@(GameState gen bTransits) = addTransitToGameState (GameState (SequentialGenerator 1 2) []) transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameStateInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b1, Just transit) $ reverse balls
  (balls, b1) = foldl fun ([], b) [1..5]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c


fakeGameState2 :: GameState
fakeGameState2 = game where
  transit = emptyTransit fakeWay2
  b@(GameState gen bTransits) = addTransitToGameState (GameState (SequentialGenerator 1 30) []) transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameStateInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b1, Just transit) $ reverse balls
  (balls, b1) = foldl fun ([], b) [1..10]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c
  fakeWay2 = Way [PointPath points] where
    points = map (\(x, y) -> IndexedPoint x y) 
      [(x, Point (fromIntegral x) 10) | x <- [0, 5 .. 500]]

fakeGameState3 :: GameState
fakeGameState3 = game where
  transit = emptyTransit fakeWay3
  b@(GameState gen bTransits) = addTransitToGameState (GameState (SequentialGenerator 1 24) []) transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameStateInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b1, Just transit) $ reverse balls
  (balls, b1) = foldl fun ([], b) [1..5]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c
  fakeWay3 = Way [PointPath points] where
    points = map (\(x, y) -> IndexedPoint x y) 
      [(x, Point (fromIntegral x) (20 * (sin (fromIntegral x)))) | x <- [0, 10..1000]]

