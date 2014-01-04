module Huma.Balls where

import Huma.Types
import Huma.Point
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.List (find, elemIndex)
import Data.List as List
import Data.Ord (comparing)

defaultBallWidth :: Width
defaultBallWidth = 20

colors :: Seq BallColor
colors = Seq.fromList [Red, Green, Blue, Yellow, Cyan, Magenta]

colorMod :: Int -> BallColor
colorMod i = Seq.index colors $ i `rem` (Seq.length colors)

ballPtFromPosition :: Position -> BallPt
ballPtFromPosition (Position ball (IndexedPoint _ pt)) = (ball, pt)

generateBall :: BallGenerator -> (Ball, BallGenerator)
generateBall (SequentialGenerator i w) = (ball, newGen) where
  ball = Ball i w $ colorMod i
  newGen = SequentialGenerator (i + 1) w

ballAndPreviousBall :: [Ball] -> [(Ball, Maybe Ball)]
ballAndPreviousBall [] = []
ballAndPreviousBall balls = zip balls $ Nothing : map Just balls

needsNewPosition :: Position -> Position -> Bool
needsNewPosition ballPos prevPos = collides || lteIndex where
  collides = isColliding ballPos prevPos 
  lteIndex = compareIndices (<=) ballPos prevPos

compareIndices :: (Index -> Index -> Bool) -> Position -> Position -> Bool
compareIndices fun (Position _ (IndexedPoint i1 _)) (Position _ (IndexedPoint i2 _)) = 
  fun i1 i2

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

freeBallToLine :: FreeBall -> Line
freeBallToLine (_, loc, orig, _) = Line orig loc


ballPosAndNextToLine :: Position -> Maybe Position -> Maybe Line
ballPosAndNextToLine (Position _ (IndexedPoint _ ptA)) 
  (Just (Position _ (IndexedPoint _ ptB))) = Just $ Line ptA ptB
ballPosAndNextToLine _ Nothing = Nothing

positionPt :: Position -> Point
positionPt (Position _ (IndexedPoint _ p)) = p

collisionDistance :: Collision -> Distance
collisionDistance (Collision _ _ dist) = dist

collisionsByDistance :: [Collision] -> [Collision]
collisionsByDistance = List.sortBy $ comparing collisionDistance

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

