module Huma.Example where

import Data.Sequence as Seq

import Huma

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
  (c, _) = foldr addBall (b1, Just transit) $ Prelude.reverse balls
  (balls, b1) = foldl fun ([], b) [1..5]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c

fakeGameState3 :: GameState
fakeGameState3 = game where
  transit = emptyTransit fakeWay3
  -- free balls
  freeBalls = map genFreeBall $ Prelude.zip free [1,4..16]

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
  (c, _) = foldr addBall (b1, Just transit) $ Prelude.reverse balls
  (balls, otherBalls) = Prelude.splitAt 6 allBalls
  (free, queued) = Prelude.splitAt 3 otherBalls
  (allBalls, b1) = foldl fun ([], b) [1..12]
  fun (balls, g) _ = case newBall g of
                       (b, g') -> (b:balls, g') 
  game = updateGameStatePositions c
  fakeWay3 = Way [PointPath points] where
    points = map (\(x, y) -> IndexedPoint x y) 
      [(x, Point (2 * (fromIntegral x)-200) (200 * (sin ((fromIntegral x) / 50.0)))) | x <- [0, 1..500]]

