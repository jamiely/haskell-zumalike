module Huma where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics.Aliases(orElse)
import Control.Monad
import System.Random (StdGen)
import Data.List (find, elemIndex)

type Width = Double
type BallId = Int
type Index = Int

data Point = Point Double Double deriving (Eq, Show)
data IndexedPoint = IndexedPoint Index Point deriving (Eq, Show)

-- a regular ball in Zuma
data Ball = Ball BallId Width deriving (Eq, Ord, Show)
-- a contiguous segment which a ball may traverse
data Path = PointPath [IndexedPoint] deriving (Eq, Show)
-- A collection of paths that feed into each other
data Way = Way [Path] deriving (Eq, Show)
data Chain = Chain [Ball] deriving (Eq, Show)
data Position = Position Ball IndexedPoint deriving (Eq, Show)
data Positions = PositionMap (Map Ball Position) deriving (Eq, Show)
-- A Transit describes a chain on a certain "Way"
data Transit = Transit Chain Way Positions deriving (Eq, Show)

data BallGenerator = SequentialGenerator Int deriving (Show)
data Game = Game BallGenerator [Transit] deriving (Show)

euclideanDistance :: Point -> Point -> Double
euclideanDistance (Point x1 y1) (Point x2 y2) = 
  sqrt $ (x1 - x2)**2 + (y1 - y2)**2

-- Returns the first way point availble on the way
firstWayPoint :: Way -> Maybe IndexedPoint
firstWayPoint (Way ((PointPath (pt:_)):_)) = Just pt
firstWayPoint _ = Nothing

defaultBallWidth :: Width
defaultBallWidth = 2

updateTransitPositions :: Transit -> Transit
updateTransitPositions (Transit c@(Chain balls) way originalPositions) =
  Transit c way newPositions where
  ballPairs = ballAndPreviousBall balls
  newPositions = foldl (flip $ updatePositionsUsingPrev way) originalPositions ballPairs

updateGamePositions :: Game -> Game
updateGamePositions (Game gen transits) = Game gen $ map updateTransitPositions transits

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
isColliding (Position (Ball _ w1) (IndexedPoint _ pt1)) 
  (Position (Ball _ w2) (IndexedPoint _ pt2)) =
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


newBallFromGenerator :: BallGenerator -> (Ball, BallGenerator)
newBallFromGenerator (SequentialGenerator i) = (ball, newGen) where
  ball = Ball i defaultBallWidth
  newGen = SequentialGenerator $ i + 1

newBall :: Game -> (Ball, Game)
newBall (Game gen transits) = (ball, game) where 
  (ball, newGen) = newBallFromGenerator gen
  game = Game newGen transits

-- Construction functions

emptyGame :: Game
emptyGame = Game (SequentialGenerator 0) []

addTransitToGame :: Game -> Transit -> Game
addTransitToGame (Game gen transits) t = Game gen (t:transits)

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

addBallToGameInTransit :: Game -> Transit -> Ball -> (Game, Maybe Transit)
addBallToGameInTransit (Game gen transits) transit ball = (game, maybeNewTransit) where
  game = Game gen newTransits 
  newTransitPairs = map update transits
  newTransits = map fst newTransitPairs 
  maybeNewTransit = liftM fst $ find snd newTransitPairs 
  -- updates the transit if it matches the `way` we are looking for
  update t = if transit == t
               then (addBallToTransit ball t, True)
               else (t, False) 

-- Setup some fake data to render

fakeBalls :: [Ball]
fakeBalls = [Ball i defaultBallWidth | i <- [1 .. 3]]

fakeWay :: Way 
fakeWay = Way [PointPath points] where
  points = map (\(x, y) -> IndexedPoint x y) 
    [(x, Point (fromIntegral x) 10) | x <- [1 .. 10]]

fakeTransit :: Transit
fakeTransit = emptyTransit fakeWay

fakeGame :: Game
fakeGame = game where
  transit = fakeTransit
  b = addTransitToGame emptyGame transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b, Just transit) fakeBalls
  game = updateGamePositions c



