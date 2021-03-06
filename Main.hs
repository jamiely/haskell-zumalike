module Main where

import Huma
import Huma.Example
import Control.Applicative
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as GlossGame
import qualified Graphics.Gloss.Data.Picture as Pic
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))

data Shooter = Shooter Pic.Point Huma.Angle
data UI = UI Shooter

width = 1200
height = 800

centerX = 600
centerY = 400

main :: IO ()
main = do
  playIO
    (InWindow "Tic-tac-toe" size position)
    azure
    10
    (fakeGameState3, UI $ Shooter (0, 0) (pi/2))
    drawGameState
    handleInput
    stepGameState where
      size = (width, height)
      position = (0, 0)

drawGameState :: (GameState, UI) -> IO Picture
drawGameState (game, ui) = do
  return $ mconcat [origin, gamePicture, shooter, drawUI ui, collisions, colAngles, ballQueuePic] where
  (GameState _ transits freeBalls ballQueue) = game
  ballQueuePic = drawBallQueue ballQueue
  gamePicture = mconcat $ map drawTransit transits
  origin = color black $ circle 10
  shooter = mconcat $ map drawFreeBall freeBalls 
  gameCollisions = collisionsByDistance $ gameStateCollisions game
  angles :: [Angle]
  angles = map (collisionAngle game) gameCollisions
  collisionsWithAngles :: [(Collision, Angle)]
  collisionsWithAngles = zip gameCollisions angles
  collisions = case gameCollisions of
                 (a:as) -> drawCollision a
                 _ -> Blank
  colAngles = mconcat $ map drawCollisionAngles collisionsWithAngles
  (GameState _ _ free _) = game

drawBallQueue :: BallQueue -> Picture
drawBallQueue que = if Seq.null que 
                      then Pic.Blank
                      else scale 0.5 0.5 $ ballPic where
  firstBall = Seq.index que $ (Seq.length que) - 1
  ballPic = drawBallAt firstBall (Point 0 0)

drawUI :: UI -> Picture
drawUI (UI (Shooter origin angle)) = 
  rotate (-(radiansToDegress angle)) $ drawPointer origin

drawPointer :: Pic.Point -> Picture
drawPointer (x, y) = rotate (0) $ Polygon [a, b, c] where 
  a = (x + 10, y + 0)
  b = (x - 5, y - 5)
  c = (x - 5, y + 5)

pointFromAngle :: Huma.Angle -> Pic.Point
pointFromAngle angle = (cos angle, sin angle)

scalePoint :: Float -> Pic.Point -> Pic.Point
scalePoint scale (x, y) = (scale * x, scale * y)

convertPoint :: Huma.Point -> Pic.Point
convertPoint (Huma.Point x y) = (x, y)

fromPoint :: Pic.Point -> Huma.Point
fromPoint (x, y) = Huma.Point x y

drawCollisionAngles :: (Collision, Angle) -> Picture
drawCollisionAngles ((Collision (_, (Huma.Point x y), _, _) _ _), angle) = t where
  t = Pic.Scale 0.5 0.5 moved
  moved = Translate x y text 
  text = Text $ show $ radiansToDegress angle

drawCollision :: Collision -> Picture
drawCollision (Collision (_, pt1, _, _) (Position _ (IndexedPoint _ pt2)) _) =  
  color black $ Pic.Line $ map convertPoint [pt1, pt2]

drawFreeBall :: FreeBall -> Picture
drawFreeBall (ball, location, origin, _) = picBall <> path where
  picBall = drawBallAt ball location
  path = color red $ Pic.Line $ map convertPoint [origin, location]

drawTransit :: Transit -> Picture
drawTransit (Transit chain way (PositionMap positionMap)) = 
  foldr (<>) Blank $ map drawPosition $ map snd $ Map.toList positionMap

drawPosition :: Position -> Picture
drawPosition (Position ball (IndexedPoint _ pt)) = drawBallAt ball pt

drawBallAt :: Ball -> Huma.Point -> Picture
drawBallAt (Ball _ width ballColor) (Point x y) = 
  translate x y $ color (ballToGlossColor ballColor) $ circle width

ballToGlossColor :: BallColor -> Color
ballToGlossColor Red = red
ballToGlossColor Green = green
ballToGlossColor Blue = blue
ballToGlossColor Yellow = yellow
ballToGlossColor Magenta = magenta
ballToGlossColor Cyan = cyan

handleInput :: Event -> (GameState, UI) -> IO (GameState, UI)
{-handleInput (EventKey (MouseButton LeftButton) Up _ (x, y))-}
  {-(game, p) = do-}
    {-let newGameState = game -- TODO: add ball to game in first transit-}
    {-return (newGameState, p)-}

handleInput (EventKey (Char 'a') Up _ _) (game, p) = do
  putStrLn "Add a ball"
  return (newGameState2, p) where
  (newGameState1, _) = addBallToGameStateInFirstTransit game
  newGameState2 = updateGameStatePositions newGameState1
  addBallToGameStateInFirstTransit g@(GameState _ (t:ts) _ _) = addNewBallToGameStateInTransit g t
  addBallToGameStateInFirstTransit g = (g, Nothing)

handleInput (EventKey (Char '?') Up _ _) (game@(GameState _ _ free _), p) = do
  putStrLn $ show free
  return (game, p)

handleInput (EventKey (Char 'n') Up _ _) (game, p) = do
  putStrLn "Tick"
  return (moveFirstBallForwardInGameStateAndUpdate game, p)

handleInput (EventKey (Char 'l') Up _ _) (game, oldUI) = return (game, newUI) where
  newUI = rotateShooterBy defaultIncAngle oldUI

handleInput (EventKey (Char 'r') Up _ _) (game, oldUI) = return (game, newUI) where
  newUI = rotateShooterBy (-defaultIncAngle) oldUI

handleInput (EventKey (Char 'x') Up _ _) (game, ui) = do
  putStrLn "Launch ball!"
  return (newGame, ui) where
    newGame = launchBall game ui

handleInput (EventKey (MouseButton LeftButton) Up _ _) (game, ui) = do
  putStrLn "Launch ball!"
  return (newGame, ui) where
    newGame = launchBall game ui

handleInput (EventMotion pos@(fX, fY)) (game, ui) = do
  return (game, rotateShooterTo angle ui) where
    angle = calculateShooterAngle pos

handleInput _ a = return a

adjOriginPoints :: (Float, Float) -> (Float, Float)
adjOriginPoints (x, y) = (x, y)

calculateShooterAngle :: (Float, Float) -> Huma.Angle
calculateShooterAngle pts = atan2 opposite adjacent where
  (adjacent, opposite) = adjOriginPoints pts

launchBall :: GameState -> UI -> GameState 
launchBall gs ui = newGameState where
  (ball, gsAfterBall) = newBall gs
  (UI (Shooter shootOrigin shootAngle)) = ui
  (GameState gen transits free queue) = gsAfterBall
  newGameState = GameState gen transits newFree newQueue
  ballQueueLength = Seq.length queue
  newQueue = ball <| (Seq.take (ballQueueLength - 1) queue)
  shootBall = Seq.index queue $ ballQueueLength - 1
  shootPt = fromPoint shootOrigin
  newFree = (shootBall, shootPt, shootPt, shootAngle):free

rotateShooterBy :: Huma.Angle -> UI -> UI
rotateShooterBy incAngle (UI (Shooter origin oldAngle)) = 
  UI $ Shooter origin (oldAngle + incAngle)

rotateShooterTo :: Huma.Angle -> UI -> UI
rotateShooterTo newAngle (UI (Shooter origin oldAngle)) = 
  UI $ Shooter origin newAngle

defaultIncAngle :: Huma.Angle 
defaultIncAngle = 2 * pi / 60

radiansToDegress :: Huma.Angle -> Huma.Angle
radiansToDegress angle = 360/2/pi * angle

stepGameState :: Float -> (GameState, UI) -> IO (GameState, UI)
stepGameState f (game, p) = return (newGameAfterMatches, p) where
  moveBalls = moveFirstBallForwardInGameStateAndUpdate game
  gameCollisions = collisionsByDistance $ gameStateCollisions game
  newGame = updateFreeBalls f moveBalls
  newGameAfterCollision = case gameCollisions of
                            (c:cs) -> processCollision newGame c
                            _      -> newGame
  (maybeMatch, newGameAfterMatches) = processMatches newGameAfterCollision

