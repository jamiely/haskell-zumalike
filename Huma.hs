module Huma where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics.Aliases(orElse)
import Control.Monad
import System.Random (StdGen)

data Point = Point Int Int deriving (Eq)
-- a regular ball in Zuma
data Ball = Ball Int deriving (Eq, Ord)
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

-- Returns the first way point availble on the way
firstWayPoint :: Way -> Maybe Point
firstWayPoint (Way ((PointPath (pt:_)):_)) = Just pt
firstWayPoint _ = Nothing

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
updatePositionitionUsingPrev _ (_, Nothing) ps = ps
updatePositionitionUsingPrev way (ball, Just previous) originalPositions = 
  if collides then newPositions else originalPositions where
    -- ball positions
    maybePrevPosition = getPosition previous originalPositions
    maybeBallPos = getPosition ball originalPositions

    collides = case maybeCollides of 
                 Just c -> c
                 Nothing -> False
    maybeCollides = liftM2 isColliding maybeBallPos maybePrevPosition

    -- update the positions with a new location
    newPositions = updatePosition newPos originalPositions
    newPos = case maybeNewPos of
               Just p -> p
               Nothing -> Position ball (Point 0 0)
    -- update the current ball position so it's not colliding
    maybeNewPos = liftM2 (nonCollidingPosition way) maybeBallPos maybePrevPosition

updatePosition :: Position -> Positions -> Positions
updatePosition (Position ball point) (PositionMap map) = PositionMap $ Map.insert ball point map 

getPosition :: Ball -> Positions -> Maybe Position
getPosition ball (PositionMap map) = maybePos where 
  maybePos = liftM (Position ball) maybePt
  maybePt = Map.lookup ball map

isColliding :: Position -> Position -> Bool
isColliding _ _ = False -- TODO

-- Returns a new ball position updated along the way so it is not colliding
-- with the first ball position.
nonCollidingPosition :: Way -> Position -> Position -> Position
nonCollidingPosition _ a _ = a -- TODO

newBallFromGenerator :: BallGenerator -> (Ball, BallGenerator)
newBallFromGenerator (SequentialGenerator i) = (ball, newGen) where
  ball = Ball i
  newGen = SequentialGenerator $ i + 1

newBall :: Game -> (Ball, Game)
newBall (Game gen transits) = (ball, game) where 
  (ball, newGen) = newBallFromGenerator gen
  game = Game newGen transits

-- Construction functions

emptyGame :: Game
emptyGame = Game (SequentialGenerator 0) []

addBallToTransit :: Ball -> Transit -> Transit
addBallToTransit ball (Transit (Chain balls) way poss) = 
  Transit newChain way newPositions where
    newChain = Chain $ ball:balls
    newPositions = case maybeNewPosition of 
                     Just newPosition -> updatePosition newPosition poss
                     Nothing -> poss
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
fakeBalls = [Ball i | i <- [1 .. 10]]

{-fakeGame :: IO StdGen -> Game-}
{-fakeGame randGen = Game gen board transits where-}
  {-gen = RandomBallGenerator randGen-}
  {-board = ways-}
  {-ways = map Way paths-}
  {-transits = map createTransit ways-}
  {-pointRange = [1..10]-}
  {-makePath y = PointPath [Point i y | i <- [1..10]]-}
  {-paths = map makePath [10, 30]-}



