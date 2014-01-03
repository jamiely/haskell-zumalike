module Huma where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics.Aliases(orElse)
import Control.Monad
import System.Random (StdGen)
import Data.List (find, elemIndex)
import Data.List as List
import Data.Sequence (Seq)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe

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
addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
pointFromPair :: (Float, Float) -> Point
pointFromPair (x, y) = Point x y

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
data Match = Match [Ball]

data BallGenerator = SequentialGenerator Index Width deriving (Show, Eq)
type Angle = Float
-- represents balls flying over the screen
type FreeBall = (Ball, Point, Point, Angle)
type BallQueue = Seq Ball
data GameState = GameState BallGenerator [Transit] 
  [FreeBall] BallQueue
  deriving (Show, Eq)
type GameTick = (GameState, Tick)
type BallPt = (Ball, Point)
type Distance = Float
data Collision = Collision FreeBall Position Distance deriving (Eq, Show)

-- list of game ticks, in order of descending time
data Game = Game [GameTick]

ballPtFromPosition :: Position -> BallPt
ballPtFromPosition (Position ball (IndexedPoint _ pt)) = (ball, pt)

generateBall :: BallGenerator -> (Ball, BallGenerator)
generateBall (SequentialGenerator i w) = (ball, newGen) where
  ball = Ball i w $ colorMod i
  newGen = SequentialGenerator (i + 1) w

euclideanDistance :: Point -> Point -> Distance
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
updateGameStatePositions (GameState gen transits free queued) = GameState gen ( map updateTransitPositions transits) free queued
 
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
isColliding (Position b1 (IndexedPoint _ pt1)) 
  (Position b2 (IndexedPoint _ pt2)) = ballPtsColliding (b1, pt1) (b2, pt2)

ballPtsColliding :: BallPt -> BallPt -> Bool
ballPtsColliding a b = fst $ ballPtsCollidingWithDist a b

ballPtsCollidingWithDist :: BallPt -> BallPt -> (Bool, Distance)
ballPtsCollidingWithDist (b1, p1) (b2, p2) = (dist < radii, dist) where
  dist = euclideanDistance p1 p2
  radii = foldr1 (+) $ map radius [b1, b2]
  radius (Ball _ w _) = w

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
wayPointGivenIndex (Way ((PointPath points):ps)) i = if (length points) > i 
                                                       then Just (points !! i)
                                                       else Nothing
wayPointGivenIndex _ _ = Nothing

newBall :: GameState -> (Ball, GameState)
newBall (GameState gen transits free queued) = (ball, game) where 
  (ball, newGen) = generateBall gen
  game = GameState newGen transits free queued

-- Construction functions

emptyGameState :: GameState
emptyGameState = GameState (SequentialGenerator 1 defaultBallWidth) [] [] Seq.empty

addTransitToGameState :: GameState -> Transit -> GameState
addTransitToGameState (GameState gen transits free queued) t = GameState gen (t:transits) free queued

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

transitPositions :: Transit -> Positions
transitPositions (Transit _ _ p) = p

gameStatePositions :: GameState -> [Positions]
gameStatePositions (GameState _ transits _ _) = map transitPositions transits

gameStateCollisions :: GameState -> [Collision]
gameStateCollisions gs@(GameState _ _ free _) = concat $ map calcCollisions free where
  calcCollisions :: FreeBall -> [Collision]
  calcCollisions freeball = concat $ map (collisions freeball) 
    $ gameStatePositions gs

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
  a = (GameState (SequentialGenerator 1 2) [] [] (Seq.empty))
  b@(GameState gen bTransits _ _) = addTransitToGameState a transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameStateInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b1, Just transit) $ reverse balls
  (balls, b1) = foldl fun ([], b) [1..5]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c

data Line = Line Point Point

freeBallToLine :: FreeBall -> Line
freeBallToLine (_, loc, orig, _) = Line orig loc

ballPosAndNextToLine :: Position -> Maybe Position -> Maybe Line
ballPosAndNextToLine (Position _ (IndexedPoint _ ptA)) 
  (Just (Position _ (IndexedPoint _ ptB))) = Just $ Line ptA ptB
ballPosAndNextToLine _ Nothing = Nothing

vectorFromLine :: Line -> Point
vectorFromLine (Line (Point x1 y1) (Point x2 y2)) = Point (x2-x1) (y2-y1)

-- we want the angle between two lines (vectors)
-- If we hit a ball and the angle is greater than 90 degrees, we
-- want to insert the new ball BEHIND the ball we hit. Otherwise,
-- it should be in front of the ball
angleBetweenVectors v1 v2 = acos $ dot / mag where
  dot = dotProduct $ Line v1 v2
  mag = foldr1 (*) $ map vectorMagnitude [v1, v2]

positionPt :: Position -> Point
positionPt (Position _ (IndexedPoint _ p)) = p

collisionAngle :: GameState -> Collision -> Angle 
collisionAngle gs (Collision freeBall position _) = angle where
  angle = angleBetweenVectors v1 v2
  v1 = vectorFromLine $ freeBallToLine freeBall
  v2 = vectorFromLine $ Line nextPt pt
  (Position _ ip@(IndexedPoint index pt)) = position
  nextPt = case nextPointFromPosition gs position of
             Just (IndexedPoint _ p) -> p
             Nothing -> Point 0 0

collisionDistance :: Collision -> Distance
collisionDistance (Collision _ _ dist) = dist

collisionsByDistance :: [Collision] -> [Collision]
collisionsByDistance = List.sortBy $ comparing collisionDistance

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

ballPositionInPositions :: Positions -> Ball -> Maybe Position
ballPositionInPositions (PositionMap pos) = flip Map.lookup $ pos

dotProduct :: Line -> Float
dotProduct (Line (Point x1 y1) (Point x2 y2)) = x1 * x2 + y1 * y2
vectorMagnitude (Point x y) = sqrt $ x ** 2 + y ** 2

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

matchCountFromHead :: [Ball] -> Int
matchCountFromHead bs@((Ball _ _ col):as) = length $ ballsWithColor bs col

-- Returns matches and the rest of the balls with the matches removed.
removeMatches :: [Ball] -> (Maybe Match, [Ball])
removeMatches balls@(b:bs) = if matchCountFromHead balls >= 3
                               then (Just $ Match (take 3 balls), drop 3 balls)
                               else case removeMatches bs of
                                      (matches, rest) -> (matches, b:rest)
removeMatches [] = (Nothing, [])

-- return all of the balls matching the passed color, starting from
-- the head of the list
ballsWithColor :: [Ball] -> BallColor -> [Ball]
ballsWithColor (a:as) col = if ballColor a == col
                              then a : ballsWithColor as col
                              else []
ballsWithColor [] _ = []

ballColor :: Ball -> BallColor
ballColor (Ball _ _ color) = color

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

fakeGameState3 :: GameState
fakeGameState3 = game where
  transit = emptyTransit fakeWay3
  -- free balls
  freeBalls = map genFreeBall $ zip free [1,4..16]

  genFreeBall (ball, seedi) = let
    seed = fromIntegral seedi
    angle = 2 * pi / seed
    speed = 25 * seed
    x = speed * cos angle
    y = speed * sin angle
    in (ball, Point x y, Point 0 0, angle)

  a = GameState (SequentialGenerator 1 24) [] freeBalls $ Seq.fromList queued
  b@(GameState gen bTransits _ _) = addTransitToGameState a transit 
  addBall ball (game, mt) = case mt of
                             Just t -> addBallToGameStateInTransit game t ball
                             Nothing -> (game, mt)
  (c, _) = foldr addBall (b1, Just transit) $ reverse balls
  (balls, otherBalls) = splitAt 6 allBalls
  (free, queued) = splitAt 3 otherBalls
  (allBalls, b1) = foldl fun ([], b) [1..12]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c
  fakeWay3 = Way [PointPath points] where
    points = map (\(x, y) -> IndexedPoint x y) 
      [(x, Point (2 * (fromIntegral x)-200) (200 * (sin ((fromIntegral x) / 50.0)))) | x <- [0, 1..500]]

