module Main where

import Huma
import Control.Applicative
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as GlossGame
import qualified Graphics.Gloss.Data.Picture as Pic
import Control.Monad
import qualified Data.Map as Map

data Shooter = Shooter Pic.Point Huma.Angle
data UI = UI Shooter

main :: IO ()
main = do
  playIO
    (InWindow "Tic-tac-toe" size position)
    azure
    10
    (fakeGameState3, UI $ Shooter (0, 0) 0)
    drawGameState
    handleInput
    stepGameState where
      size = (1200, 800)
      position = (0, 0)

drawGameState :: (GameState, UI) -> IO Picture
{-drawGameState (game, _) = return (grid <> plays)-}
drawGameState (game, ui) = return $ origin <> gamePicture <> shooter <> drawUI ui where
  (GameState _ transits freeBalls _) = game
  gamePicture = translate (-200) 200 $ mconcat $ map drawTransit transits
  origin = color black $ circle 10
  shooter = mconcat $ map drawFreeBall freeBalls 

drawUI :: UI -> Picture
drawUI (UI (Shooter origin angle)) = rotate (-(radiansToDegress angle)) $ drawPointer origin

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

drawFreeBall :: FreeBall -> Picture
drawFreeBall (ball, location, origin, _) = picBall <> path where
  picBall = drawBallAt ball location
  path = color red $ Line $ map convertPoint [origin, location]

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
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y))
  (game, p) = do
    let newGameState = game -- TODO: add ball to game in first transit
    return (newGameState, p)

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

handleInput _ a = return a

launchBall :: GameState -> UI -> GameState 
launchBall gs ui = newGameState where
  (ball, gsAfterBall) = newBall gs
  (UI (Shooter shootOrigin shootAngle)) = ui
  (GameState gen transits free queue) = gsAfterBall
  newGameState = GameState gen transits newFree queue
  shootPt = fromPoint shootOrigin
  newFree = (ball, shootPt, shootPt, shootAngle):free

rotateShooterBy :: Huma.Angle -> UI -> UI
rotateShooterBy incAngle (UI (Shooter origin oldAngle)) = 
  UI $ Shooter origin (oldAngle + incAngle)

defaultIncAngle :: Huma.Angle 
defaultIncAngle = 2 * pi / 60

radiansToDegress :: Huma.Angle -> Huma.Angle
radiansToDegress angle = 360/2/pi * angle

stepGameState :: Float -> (GameState, UI) -> IO (GameState, UI)
stepGameState f (game, p) = return (newGame, p) where
  moveBalls = moveFirstBallForwardInGameStateAndUpdate game
  newGame = updateFreeBalls f moveBalls

