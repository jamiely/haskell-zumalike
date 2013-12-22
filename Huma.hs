module Huma where

import Data.Map (Map)

data Point = Point Int Int
-- a regular ball in Zuma
data Ball = Ball
-- a contiguous segment which a ball may traverse
data Path = PointPath [Point]
-- A collection of paths that feed into each other
data Way = Way [Path]
data Chain = Chain [Ball]
data Board = Board [Way]
data Positions = PositionMap (Map Ball Point)
-- A Transit describes a chain on a certain "Way"
data Transit = Transit Chain Way Positions

data Game = Game Board [Chain]
data Position = Position Ball Point

-- the game progresses as balls are added to a chain, and 
-- push other balls forward on their respective way
addBall :: Transit -> Transit
addBall (Transit (Chain balls) way originalPositions) = 
  Transit newChain way newPositions where
  newBall = Ball
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
    prevPosition = getPosition previous originalPositions
    ballPos = getPosition ball originalPositions

    collides = isColliding ballPos prevPosition
    -- update the positions with a new location
    newPositions = updatePosition newPos originalPositions
    -- update the current ball position so it's not colliding
    newPos = nonCollidingPosition prevPosition ballPos way

updatePosition :: Position -> Positions -> Positions
updatePosition _ b = b -- TODO

getPosition :: Ball -> Positions -> Position
getPosition a _ = Position a $ Point 1 1 -- TODO 

isColliding :: Position -> Position -> Bool
isColliding _ _ = False -- TODO

-- Returns a new ball position updated along the way so it is not colliding
-- with the first ball position.
nonCollidingPosition :: Position -> Position -> Way -> Position
nonCollidingPosition a _ _ = a -- TODO

