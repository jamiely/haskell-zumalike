module Main where

import System.Exit (exitFailure)
import Data.Monoid
import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import qualified Test.HUnit as HUnit
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
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

testFakeGameState :: HUnit.Test
testFakeGameState = TestCase $ assertEqual "fakeGameState" ex act where
    ex = GameState (SequentialGenerator 6 2.0) transits [] Seq.empty
    transits = [Transit (Chain [Ball 1 2.0 Green,Ball 2 2.0 Blue,Ball 3 2.0 Yellow,Ball 4 2.0 Cyan,Ball 5 2.0 Magenta]) (Way [PointPath [IndexedPoint 0 (Point 0.0 10.0),IndexedPoint 5 (Point 5.0 10.0),IndexedPoint 10 (Point 10.0 10.0),IndexedPoint 15 (Point 15.0 10.0),IndexedPoint 20 (Point 20.0 10.0),IndexedPoint 25 (Point 25.0 10.0),IndexedPoint 30 (Point 30.0 10.0)]]) (PositionMap (Map.fromList [(Ball 1 2.0 Green,Position (Ball 1 2.0 Green) (IndexedPoint 0 (Point 0.0 10.0))),(Ball 2 2.0 Blue,Position (Ball 2 2.0 Blue) (IndexedPoint 5 (Point 5.0 10.0))),(Ball 3 2.0 Yellow,Position (Ball 3 2.0 Yellow) (IndexedPoint 10 (Point 10.0 10.0))),(Ball 4 2.0 Cyan,Position (Ball 4 2.0 Cyan) (IndexedPoint 15 (Point 15.0 10.0))),(Ball 5 2.0 Magenta,Position (Ball 5 2.0 Magenta) (IndexedPoint 20 (Point 20.0 10.0)))]))]
    act = fakeGameState

testMoveBallUp :: HUnit.Test
testMoveBallUp = TestList [
    TestCase $ assertEqual "wayPointGivenIndex" 
      (Just ip2) (wayPointGivenIndex way 1),
    TestCase $ assertEqual "incrementPointIndex" 
      (Just ip2) (incrementPointIndex way ip1),
    TestCase $ assertEqual "incrementPointIndex" 
      (Just ip4) (incrementPointIndex way ip3),
    TestCase $ assertEqual "moveFirstBallForwardInTransit" ex1 act1
  ] where
  chain = Chain [ball1, ball2, ball3]
  ip1 = IndexedPoint 0 $ origin
  ip2 = IndexedPoint 1 $ Point 5.0 1.0
  ip3 = IndexedPoint 2 $ Point 9.0 1.0
  ip4 = IndexedPoint 3 $ Point 13.0 1.0
  
  points = [ip1, ip2, ip3, ip4]
  way = Way [PointPath points]
  baseTransit = Transit chain way (PositionMap originalPositions)
  originalPositions = Map.fromList [(ball1, Position ball1 ip1),
                                    (ball2, Position ball2 ip2),
                                    (ball3, Position ball3 ip3)]
  ex1 = Transit chain way (PositionMap expectedPositions)
  expectedPositions = Map.fromList [(ball1, Position ball1 ip2),
                                    (ball2, Position ball2 ip2),
                                    (ball3, Position ball3 ip3)]
  act1 = moveFirstBallForwardInTransit baseTransit

testCollisions :: HUnit.Test
testCollisions = TestCase $ assertEqual "collisions" ex act where
  ballA = Ball 1 2.0 Red
  ballB = Ball 2 2.0 Red
  ballPtA = (ballA, Point 0 0)
  pos = Position ballB (IndexedPoint 0 (Point 3.5 0))
  freeball = (ballA, Point 0 0, Point 0 0, 0)
  ex = [Collision freeball pos]
  act = collisions freeball positions
  positions = PositionMap $ Map.fromList [(ballB, pos)]

-- Some Tests
tests = TestList [ 
    testBallAndPreviousBall,
    testNonCollidingPosition,
    testNonCollidingPositionAlongPoints,
    testUpdatePositionsUsingPrev,
    testUpdatePositionsUsingPrev2,
    testUpdateTransitPositions,
    testFakeGameState,
    testMoveBallUp
  ] where

main :: IO ()
main = defaultMainWithOpts
       [testGroup "Hunit tests" hunitTests] mempty where
       hunitTests = hUnitTestToTests tests

