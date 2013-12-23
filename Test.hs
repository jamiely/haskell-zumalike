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

ball1 = Ball 1 2.0
ball2 = Ball 2 2.0
ball3 = Ball 3 2.0
balls = [ball1, ball2, ball3]

origin :: Point
origin = Point 1.0 1.0

testWay :: Way
testWay = Way [PointPath points] where
  points = [Point x 1.0 | x <- [1 .. 10]]

testNonCollidingPosition :: HUnit.Test
testNonCollidingPosition = TestCase assertion where
  assertion = assertEqual "Non colliding position" expectedPosition actualPosition
  posWithBall ball = Position ball origin
  expectedPosition = Just $ Position ball2 $ Point 5.0 1.0
  actualPosition = nonCollidingPosition testWay p1 p2 
  p1 = posWithBall ball1
  p2 = posWithBall ball2

testBallAndPreviousBall :: HUnit.Test
testBallAndPreviousBall = TestCase $ assertEqual "ball and previous ball" 
  [(ball1, Nothing), (ball2, Just ball1), (ball3, Just ball2)]
  $ ballAndPreviousBall balls

testUpdatePositionsUsingPrev :: HUnit.Test
testUpdatePositionsUsingPrev = TestCase $ assertEqual "updatePositionsUsingPrev" ex act where
  exWay = testWay
  testPositions = PositionMap $ Map.fromList [(ball2, Point 5.0 1.0),
                                              (ball3, Point 5.0 1.0)]
  ex = PositionMap $ Map.fromList [(ball2, Point 5.0 1.0),
                                   (ball3, Point 9.0 1.0)]
  act = updatePositionsUsingPrev exWay (ball3, Just ball2) testPositions

testUpdateTransitPositions :: HUnit.Test
testUpdateTransitPositions = TestCase $ assertEqual "updateTransitPositions" ex act where
  exWay = testWay
  exChain = Chain balls
  testPositions = PositionMap $ Map.fromList $ map (\b -> (b, origin)) balls
  testTransit = Transit exChain exWay testPositions
  exPositions = PositionMap $ Map.fromList [(ball1, origin),
                                            (ball2, Point 5.0 1.0),
                                            (ball3, Point 9.0 1.0)]
  ex = Transit exChain exWay exPositions
  act = updateTransitPositions testTransit

testFakeGame :: HUnit.Test
testFakeGame = TestCase $ assertEqual "Games" 
    "Game (SequentialGenerator 0) [Transit (Chain [Ball 1 2.0,Ball 2 2.0,Ball 3 2.0]) (Way [PointPath [Point 1.0 10.0,Point 2.0 10.0,Point 3.0 10.0,Point 4.0 10.0,Point 5.0 10.0,Point 6.0 10.0,Point 7.0 10.0,Point 8.0 10.0,Point 9.0 10.0,Point 10.0 10.0]]) (PositionMap (fromList [(Ball 1 2.0,Point 1.0 10.0),(Ball 2 2.0,Point 5.0 10.0),(Ball 3 2.0,Point 10.0 10.0)]))]"
    $ show fakeGame

-- Some Tests
tests = TestList [ 
    testBallAndPreviousBall,
    testNonCollidingPosition,
    testUpdatePositionsUsingPrev,
    testUpdateTransitPositions,
    testFakeGame
  ] where

main :: IO ()
main = defaultMainWithOpts
       [testGroup "Hunit tests" hunitTests] mempty where
       hunitTests = hUnitTestToTests tests
