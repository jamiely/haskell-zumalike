module Huma.Transits where

import Huma.Types
import Huma.Point
import Huma.Balls

import qualified Data.Map as Map
import Control.Monad (liftM)

-- Returns the first way point availble on the way
firstWayPoint :: Way -> Maybe IndexedPoint
firstWayPoint (Way ((PointPath (pt:_)):_)) = Just pt
firstWayPoint _ = Nothing

updateTransitPositions :: Transit -> Transit
updateTransitPositions (Transit c@(Chain balls) way originalPositions) =
  Transit c way newPositions where
  ballPairs = ballAndPreviousBall balls
  newPositions = foldl (flip $ updatePositionsUsingPrev way) originalPositions ballPairs

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

updatePosition :: Position -> Positions -> Positions
updatePosition p@(Position ball point) (PositionMap map) = 
  PositionMap $ Map.insert ball p map 

getPosition :: Positions -> Ball -> Maybe Position
getPosition (PositionMap map) ball = Map.lookup ball map where 


-- Returns a new ball position updated along the way so it is not colliding
-- with the first ball position.
nonCollidingPosition :: Way -> Position -> Position -> Maybe Position
nonCollidingPosition (Way (path:paths)) noCollide pos = 
  case nonCollidingPositionAlongPath path noCollide pos of
    newPos@(Just _) -> newPos
    Nothing -> nonCollidingPosition (Way paths) noCollide pos
nonCollidingPosition (Way []) _ _ = Nothing


incrementPointIndex :: Way -> IndexedPoint -> Maybe IndexedPoint
incrementPointIndex way (IndexedPoint index pt) = wayPointGivenIndex way $ index + 1

wayPointGivenIndex :: Way -> Index -> Maybe IndexedPoint
wayPointGivenIndex (Way ((PointPath points):ps)) i = if (length points) > i 
                                                       then Just (points !! i)
                                                       else Nothing
wayPointGivenIndex _ _ = Nothing

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

moveFirstBallForwardInTransit :: Transit -> Transit
moveFirstBallForwardInTransit (Transit c@(Chain (b:bs)) way (PositionMap positions)) = newTransit where
  newTransit = Transit c way (PositionMap newPositions)
  newPositions = case maybeNewPositions of
                   Just p -> p
                   Nothing -> positions
  -- yield the previous position if there is a problem.
  maybeNewPositions = do
    -- todo: use Map.update here.
    (Position _ ip) <- Map.lookup b positions
    newIP <- incrementPointIndex way ip
    return $ Map.insert b (Position b newIP) positions 


transitPositions :: Transit -> Positions
transitPositions (Transit _ _ p) = p


-- Returns all collisions for a point
collisions :: FreeBall -> Positions -> [Collision]
collisions freeBall (PositionMap positionsMap) = 
  map colFun
  $ filter (fst . snd)
  $ map mapFun
  $ Map.elems positionsMap where
  colFun (pos, (_, dist)) = Collision freeBall pos dist
  (ball, pt, _, _) = freeBall
  ballpt = (ball, pt)
  mapFun pos = (pos, ballPtsCollidingWithDist ballpt $ ballPtFromPosition pos)

ballPositionInPositions :: Positions -> Ball -> Maybe Position
ballPositionInPositions (PositionMap pos) = flip Map.lookup $ pos

processMatchesInTransit :: Transit -> (Maybe Match, Transit)
processMatchesInTransit (Transit (Chain balls) w (PositionMap oldPositions)) = (maybeMatch, newTransit) where
  (maybeMatch, newBalls) = removeMatches balls
  -- TODO: we should remove the balls from the position map as well
  maybeNewPositions = do
    (Match balls) <- maybeMatch
    return $ Map.filterWithKey (\k a -> not $ elem k balls) oldPositions
  newPositions = case maybeNewPositions of 
                   Just p -> p
                   Nothing -> oldPositions
  newTransit = Transit (Chain newBalls) w (PositionMap newPositions)

