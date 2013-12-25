module Main where

import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import qualified Test.HUnit as HUnit
import qualified Data.Map as Map
import Test.QuickCheck
import Huma

ball1 = Ball 1 2.0 Red
ball2 = Ball 2 2.0 Red
ball3 = Ball 3 2.0 Red
balls = [ball1, ball2, ball3]

origin :: Point
origin = Point 1.0 1.0

indexedOrigin = IndexedPoint 1 origin
indexedPoint2 = IndexedPoint 5 $ Point 5.0 1.0
indexedPoint3 = IndexedPoint 9 $ Point 9.0 1.0

testIndexedPoints :: [IndexedPoint]
testIndexedPoints = map (\(x, y) -> IndexedPoint x y) 
  [(x, Point (fromIntegral x) 1.0) | x <- [1 .. 10]]

testPoints :: [Point]
testPoints = map (\(IndexedPoint _ pt) -> pt) testIndexedPoints

testWay :: Way
testWay = Way [PointPath testIndexedPoints]

testNonCollidingPosition :: HUnit.Test
testNonCollidingPosition = TestCase assertion where
  assertion = assertEqual "Non colliding position" expectedPosition actualPosition
  posWithBall ball = Position ball indexedOrigin
  expectedPosition = Just $ Position ball2 indexedPoint2
  actualPosition = nonCollidingPosition testWay p1 p2 
  p1 = posWithBall ball1
  p2 = posWithBall ball2

testNonCollidingPositionAlongPoints :: HUnit.Test
testNonCollidingPositionAlongPoints = TestCase assertion where
  assertion = assertEqual "nonCollidingPositionAlongPoints" ex act
  ex = Just $ Position ball3 indexedPoint3
  act = nonCollidingPositionAlongPoints testIndexedPoints
    (Position ball2 indexedPoint2)
    $ Position ball3 indexedOrigin

testBallAndPreviousBall :: HUnit.Test
testBallAndPreviousBall = TestCase $ assertEqual "ballAndPreviousBall" 
  [(ball1, Nothing), (ball2, Just ball1), (ball3, Just ball2)]
  $ ballAndPreviousBall balls

testUpdatePositionsUsingPrev :: HUnit.Test
testUpdatePositionsUsingPrev = TestCase $ assertEqual "updatePositionsUsingPrev" ex act where
  exWay = testWay
  testPositions = PositionMap $ Map.fromList [(ball1, Position ball1 indexedOrigin),
                                              (ball2, Position ball2 indexedPoint2),
                                              (ball3, Position ball3 indexedPoint2)]
  ex = PositionMap $ Map.fromList [(ball1, Position ball1 indexedOrigin),
                                   (ball2, Position ball2 indexedPoint2),
                                   (ball3, Position ball3 indexedPoint3)]
  act = updatePositionsUsingPrev exWay (ball3, Just ball2) testPositions

testUpdatePositionsUsingPrev2 :: HUnit.Test
testUpdatePositionsUsingPrev2 = TestCase $ assertEqual "updatePositionsUsingPrev2" ex act where
  exWay = testWay
  testPositions = PositionMap $ Map.fromList [(ball1, Position ball1 indexedOrigin),
                                              (ball2, Position ball2 indexedPoint2),
                                              (ball3, Position ball3 indexedOrigin)]
  ex = PositionMap $ Map.fromList [(ball1, Position ball1 indexedOrigin),
                                   (ball2, Position ball2 indexedPoint2),
                                   (ball3, Position ball3 indexedPoint3)]
  act = updatePositionsUsingPrev exWay (ball3, Just ball2) testPositions

testUpdateTransitPositions :: HUnit.Test
testUpdateTransitPositions = TestCase $ assertEqual "updateTransitPositions" ex act where
  exWay = testWay
  exChain = Chain balls
  testPositions = PositionMap $ Map.fromList $ map (\b -> (b, Position b indexedOrigin)) balls
  testTransit = Transit exChain exWay testPositions
  exPositions = PositionMap $ Map.fromList [(ball1, Position ball1 indexedOrigin),
                                            (ball2, Position ball2 indexedPoint2),
                                            (ball3, Position ball3 indexedPoint3)]
  ex = Transit exChain exWay exPositions
  act = updateTransitPositions testTransit

testFakeGame :: HUnit.Test
testFakeGame = TestCase $ assertEqual "fakeGame" ex act where
    ex = Game (SequentialGenerator 6 2.0) [Transit (Chain [Ball 1 2.0 Red,Ball 2
      2.0 Red,Ball 3 2.0 Red,Ball 4 2.0 Red,Ball 5 2.0 Red]) (Way [PointPath [IndexedPoint 0
      (Point 0.0 10.0),IndexedPoint 5 (Point 5.0 10.0),IndexedPoint 10 (Point
      10.0 10.0),IndexedPoint 15 (Point 15.0 10.0),IndexedPoint 20 (Point 20.0
      10.0),IndexedPoint 25 (Point 25.0 10.0),IndexedPoint 30 (Point 30.0
      10.0)]]) (PositionMap (Map.fromList [(Ball 1 2.0 Red,Position (Ball 1 2.0 Red)
      (IndexedPoint 0 (Point 0.0 10.0))),(Ball 2 2.0 Red,Position (Ball 2 2.0 Red)
      (IndexedPoint 5 (Point 5.0 10.0))),(Ball 3 2.0 Red,Position (Ball 3 2.0 Red)
      (IndexedPoint 10 (Point 10.0 10.0))),(Ball 4 2.0 Red,Position (Ball 4 2.0 Red)
      (IndexedPoint 15 (Point 15.0 10.0))),(Ball 5 2.0 Red,Position (Ball 5 2.0 Red)
      (IndexedPoint 20 (Point 20.0 10.0)))]))]
    act = fakeGame

-- Some Tests
tests = TestList [ 
    testBallAndPreviousBall,
    testNonCollidingPosition,
    testNonCollidingPositionAlongPoints,
    testUpdatePositionsUsingPrev,
    testUpdatePositionsUsingPrev2,
    testUpdateTransitPositions,
    testFakeGame
  ] where

main :: IO ()
main = defaultMainWithOpts
       [testGroup "Hunit tests" hunitTests] mempty where
       hunitTests = hUnitTestToTests tests

