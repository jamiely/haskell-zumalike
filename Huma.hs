module Huma where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics.Aliases(orElse)
import Control.Monad
import System.Random (StdGen)

type Width = Double
type BallId = Int

data Point = Point Double Double deriving (Eq)
-- a regular ball in Zuma
data Ball = Ball BallId Width deriving (Eq, Ord)
-- a contiguous segment which a ball may traverse
data Path = PointPath [Point] deriving (Eq)
-- A collection of paths that feed into each other
data Way = Way [Path] deriving (Eq)
data Chain = Chain [Ball]
data Positions = PositionMap (Map Ball Point)
-- A Transit describes a chain on a certain "Way"
data Transit = Transit Chain Way Positions

data BallGenerator = SequentialGenerator Int
data Game = Game BallGenerator [Transit]
data Position = Position Ball Point

euclideanDistance :: Point -> Point -> Double
euclideanDistance (Point x1 y1) (Point x2 y2) = 
  sqrt $ (x1 - x2)**2 + (y1 - y2)**2

-- Returns the first way point availble on the way
firstWayPoint :: Way -> Maybe Point
firstWayPoint (Way ((PointPath (pt:_)):_)) = Just pt
firstWayPoint _ = Nothing

defaultBallWidth :: Width
defaultBallWidth = 15

-- the game progresses as balls are added to a chain, and 
-- push other balls forward on their respective way
addBall :: Ball -> Transit -> Transit
addBall newBall (Transit (Chain balls) way originalPositions) = 
  Transit newChain way newPositions where
  -- add the ball to the chain
  newChain = Chain $ newBall : balls

  -- update the position of each ball, given the newball
  -- with each original ball in sequence, determine if there is a
  -- collision with the new position of the previous ball. if there is,
  -- then push the ball forward along the way until there is no collision.
  newPositions = foldr fun originalPositions $ ballAndPreviousBall balls
  fun = updatePositionitionUsingPrev way

ballAndPreviousBall :: [Ball] -> [(Ball, Maybe Ball)]
ballAndPreviousBall [] = []
ballAndPreviousBall balls = zip balls $ Nothing : map Just balls

updatePositionitionUsingPrev :: Way -> (Ball, Maybe Ball) -> Positions -> Positions
updatePositionitionUsingPrev way (ball, Just previous) originalPositions = 
  case maybePositions of
    Just p -> p
    Nothing -> originalPositions 
    where
      lookup = getPosition originalPositions

      maybePositions :: Maybe Positions
      maybePositions = do
        prevPos <- lookup previous
        ballPos <- lookup ball
        newPos <- if isColliding ballPos prevPos
                      then Just $ nonCollidingPosition way ballPos prevPos
                      else Nothing
        return $ updatePosition newPos originalPositions
updatePositionitionUsingPrev _ (_, Nothing) ps = ps

updatePosition :: Position -> Positions -> Positions
updatePosition (Position ball point) (PositionMap map) = PositionMap $ Map.insert ball point map 

getPosition :: Positions -> Ball -> Maybe Position
getPosition (PositionMap map) ball = maybePos where 
  maybePos = liftM (Position ball) maybePt
  maybePt = Map.lookup ball map

isColliding :: Position -> Position -> Bool
isColliding (Position (Ball _ w1) pt1) (Position (Ball _ w2) pt2) =
  (euclideanDistance pt1 pt2) < (w1 + w2)


-- Returns a new ball position updated along the way so it is not colliding
-- with the first ball position.
nonCollidingPosition :: Way -> Position -> Position -> Position
nonCollidingPosition _ a _ = a -- TODO

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

-- Adds a ball to the passed transit structure, setting its position to the
-- first waypoint in the way. The transit should be updated to adjust all
-- positions as soon as possible.
addBallToTransit :: Ball -> Transit -> Transit
addBallToTransit ball (Transit (Chain balls) way positions) = 
  Transit newChain way newPositions where
    newChain = Chain $ ball:balls
    newPositions = case maybeNewPosition of 
                     Just newPosition -> updatePosition newPosition positions
                     Nothing -> positions
    maybeNewPosition = liftM (Position ball) $ firstWayPoint way

addBallToGameInWay :: Game -> Ball -> Way -> Game
addBallToGameInWay (Game gen transits) ball way = Game gen newTransits where
  newTransits = map update transits
  -- updates the transit if it matches the `way` we are looking for
  update t@(Transit _ way' _) = if (way == way')
                               then (addBallToTransit ball t)
                               else t 

-- Setup some fake data to render

fakeBalls :: [Ball]
fakeBalls = [Ball i defaultBallWidth | i <- [1 .. 10]]

{-fakeGame :: IO StdGen -> Game-}
{-fakeGame randGen = Game gen board transits where-}
  {-gen = RandomBallGenerator randGen-}
  {-board = ways-}
  {-ways = map Way paths-}
  {-transits = map createTransit ways-}
  {-pointRange = [1..10]-}
  {-makePath y = PointPath [Point i y | i <- [1..10]]-}
  {-paths = map makePath [10, 30]-}



