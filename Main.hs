module Main where

import Huma
import Control.Applicative
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import qualified Data.Map as Map

data Play = Play

main :: IO ()
main = do
  playIO
    (InWindow "Tic-tac-toe" size position)
    azure
    10
    (fakeGameState3, Play)
    drawGameState
    handleInput
    stepGameState where
      size = (1200, 800)
      position = (0, 0)

drawGameState :: (GameState, Play) -> IO Picture
{-drawGameState (game, _) = return (grid <> plays)-}
drawGameState (game, _) = return (grid <> translate (-200) (200) gamePicture) where
  grid = 
    color black (line [ (-100, -300), (-100,  300) ]) <>
    color black (line [ ( 100, -300), ( 100,  300) ]) <>
    color black (line [ (-300,  100), ( 300,  100) ]) <>
    color black (line [ (-300, -100), ( 300, -100) ]) <>
    color blue (circle 10)
  
  (GameState _ transits _ _) = game
  gamePicture = foldr (<>) Blank $ map drawTransit transits

drawTransit :: Transit -> Picture
drawTransit (Transit chain way (PositionMap positionMap)) = 
  foldr (<>) Blank $ map drawPosition $ map snd $ Map.toList positionMap

drawPosition :: Position -> Picture
drawPosition (Position (Ball _ width ballColor) (IndexedPoint _ (Point x y))) = 
  translate x y $ color (ballToGlossColor ballColor) $ circle width

ballToGlossColor :: BallColor -> Color
ballToGlossColor Red = red
ballToGlossColor Green = green
ballToGlossColor Blue = blue
ballToGlossColor Yellow = yellow
ballToGlossColor Magenta = magenta
ballToGlossColor Cyan = cyan

handleInput :: Event -> (GameState, Play) -> IO (GameState, Play)
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
handleInput (EventKey (Char '?') Up _ _) (game, p) = do
  putStrLn $ show game
  return (game, p)

handleInput (EventKey (Char 'n') Up _ _) (game, p) = do
  putStrLn "Tick"
  return (moveFirstBallForwardInGameStateAndUpdate game, p)
  
handleInput _ a = return a

stepGameState :: Float -> (GameState, Play) -> IO (GameState, Play)
stepGameState f (game, p) = 
  return (moveFirstBallForwardInGameStateAndUpdate game, p)
