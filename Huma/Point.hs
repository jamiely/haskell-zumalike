module Huma.Point where

import Huma.Types

addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointFromPair :: (Float, Float) -> Point
pointFromPair (x, y) = Point x y

euclideanDistance :: Point -> Point -> Distance
euclideanDistance (Point x1 y1) (Point x2 y2) = 
  sqrt $ (x1 - x2)**2 + (y1 - y2)**2

vectorFromLine :: Line -> Point
vectorFromLine (Line (Point x1 y1) (Point x2 y2)) = Point (x2-x1) (y2-y1)

-- we want the angle between two lines (vectors)
-- If we hit a ball and the angle is greater than 90 degrees, we
-- want to insert the new ball BEHIND the ball we hit. Otherwise,
-- it should be in front of the ball
angleBetweenVectors v1 v2 = acos $ dot / mag where
  dot = dotProduct $ Line v1 v2
  mag = foldr1 (*) $ map vectorMagnitude [v1, v2]


dotProduct :: Line -> Float
dotProduct (Line (Point x1 y1) (Point x2 y2)) = x1 * x2 + y1 * y2
vectorMagnitude (Point x y) = sqrt $ x ** 2 + y ** 2

