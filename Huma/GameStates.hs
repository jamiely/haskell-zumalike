module Huma.GameStates where

import Huma.Types
import Huma.Point
import Huma.Balls
import Huma.Transits

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM)
import System.Random (StdGen)
import Data.List (find)
import Data.Sequence (Seq)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe

updateGameStatePositions :: GameState -> GameState
updateGameStatePositions (GameState gen transits free queued) = GameState gen ( map updateTransitPositions transits) free queued
 
newBall :: GameState -> (Ball, GameState)
newBall (GameState gen transits free queued) = (ball, game) where 
  (ball, newGen) = generateBall gen
  game = GameState newGen transits free queued

-- Construction functions

emptyGameState :: GameState
emptyGameState = GameState (SequentialGenerator 1 defaultBallWidth) [] [] Seq.empty

addTransitToGameState :: GameState -> Transit -> GameState
addTransitToGameState (GameState gen transits free queued) t = GameState gen (t:transits) free queued

addBallToGameStateInTransit :: GameState -> Transit -> Ball -> (GameState, Maybe Transit)
addBallToGameStateInTransit (GameState gen transits free queued) transit ball = (game, maybeNewTransit) where
  game = GameState gen newTransits free queued
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

moveFirstBallForwardInGameState :: GameState -> GameState
moveFirstBallForwardInGameState (GameState gen transits free queued) = 
  GameState gen (map moveFirstBallForwardInTransit transits) free queued

moveFirstBallForwardInGameStateAndUpdate :: GameState -> GameState 
moveFirstBallForwardInGameStateAndUpdate = updateGameStatePositions . moveFirstBallForwardInGameState

updateFreeBalls :: Float -> GameState -> GameState
updateFreeBalls travelTime (GameState gen transits free queued) = newState where
  newState = GameState gen transits newFree queued 
  -- we need to update these relative to some travel time
  speed = 5
  distance = travelTime * speed
  newFree = map updateFree free
  -- assuming angle is correct
  speedParts angle = pointFromPair (speed * cos angle, speed * sin angle)
  updateFree (ball, loc, orig, angle) = 
    (ball, addPoints loc (speedParts angle), orig, angle)

gameStatePositions :: GameState -> [Positions]
gameStatePositions (GameState _ transits _ _) = map transitPositions transits

gameStateCollisions :: GameState -> [Collision]
gameStateCollisions gs@(GameState _ _ free _) = concat $ map calcCollisions free where
  calcCollisions :: FreeBall -> [Collision]
  calcCollisions freeball = concat $ map (collisions freeball) 
    $ gameStatePositions gs

collisionAngle :: GameState -> Collision -> Angle 
collisionAngle gs (Collision freeBall position _) = angle where
  angle = angleBetweenVectors v1 v2
  v1 = vectorFromLine $ freeBallToLine freeBall
  v2 = vectorFromLine $ Line nextPt pt
  (Position _ ip@(IndexedPoint index pt)) = position
  nextPt = case nextPointFromPosition gs position of
             Just (IndexedPoint _ p) -> p
             Nothing -> Point 0 0

nextPointFromPosition :: GameState -> Position -> Maybe IndexedPoint
nextPointFromPosition (GameState _ transits _ _) (Position ball ip) = do
  transit <- find transitContainsBall transits
  inc transit where
    transitContainsBall (Transit _ _ positions) = 
      positionsContainsBall positions ball

    inc (Transit chain way positions) = incrementPointIndex way ip

    positionsContainsBall (PositionMap m) = flip Map.member $ m

ballPositionInGameState :: GameState -> Ball -> Maybe Position 
ballPositionInGameState gs ball = do
  m <- find (Map.member ball) $ map toMap $ gameStatePositions gs
  pos <- Map.lookup ball m 
  return pos where
    toMap (PositionMap m) = m

transitContainingBall :: GameState -> Ball -> Maybe Transit
transitContainingBall (GameState _ ts _ _) ball = find fun ts where
  fun (Transit (Chain balls) _ _) = elem ball balls

-- Finds matches in any chains and removes the balls. Matches are
-- where there are three or more balls in the same chain with the 
-- same color in a row
processMatches :: GameState -> (Maybe Match, GameState)
processMatches gs = (match, newGs) where
  (GameState gen transits free queue) = gs
  matchesAndTransits = map processMatchesInTransit transits
  match = case find Maybe.isJust $ map fst matchesAndTransits of
            Just m -> m
            Nothing -> Nothing
  newTransits = map snd matchesAndTransits
  newGs = GameState gen newTransits free queue

processCollision :: GameState -> Collision -> GameState
-- when a collision happens, the ball should either be inserted in front of,
-- or behind the ball it collided with.
processCollision gs col@(Collision freeBall@(newBall, _, _, _) position _) = newGs where
  newGs = updateGameStatePositions $ GameState gen newTransits newFree queued
  angle = collisionAngle gs col
  before = angle < pi/2.0
  -- modify the chain
  oldTransit@(Transit oldChain way oldPositions) =
    case transitContainingBall gs existingBall of
      Just t -> t
      Nothing -> head transits
    
  newTransit = Transit newChain way newPositions 
  (GameState gen transits free queued) = gs
  newFree = foldr (\f@(ball, _, _, _) fs -> if ball == newBall 
                                            then fs
                                            else f:fs) [] free 
  newTransits = map (\t -> if t == oldTransit 
                             then newTransit
                             else t) transits
  (Position existingBall existingIp) = position
  (Chain oldBalls) = oldChain 
  newChain = Chain $ reverse $ 
    foldr (\b bs -> if b == existingBall
                       then if before
                              then newBall : b : bs
                              else b : newBall : bs
                       else b : bs)
          [] $ reverse oldBalls
  (PositionMap mapOldPos) = oldPositions 
  newPositions = case maybeNewPos of 
                   Just newPosition -> PositionMap $ Map.insert newBall newPosition mapOldPos 
                   Nothing -> oldPositions

  -- the indexed point for the new ball
  maybeNewIP = if before
                 then Just existingIp
                 else incrementPointIndex way existingIp
  -- the position for the new ball
  maybeNewPos = do
    newIp <- maybeNewIP
    return $ Position newBall newIp
