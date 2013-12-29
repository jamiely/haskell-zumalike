module Main where

import Huma
import Control.Applicative
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as GlossGame
import qualified Graphics.Gloss.Data.Picture as Pic
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
drawGameState (game, _) = return $ origin <> gamePicture <> shooter where
  (GameState _ transits freeBalls _) = game
  gamePicture = translate (-200) 200 $ mconcat $ map drawTransit transits
  origin = color black $ circle 10
  shooter = mconcat $ map drawFreeBall freeBalls 

convertPoint :: Huma.Point -> Pic.Point
convertPoint (Huma.Point x y) = (x, y)

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
handleInput (EventKey (Char '?') Up _ _) (game@(GameState _ _ free _), p) = do
  putStrLn $ show free
  return (game, p)

handleInput (EventKey (Char 'n') Up _ _) (game, p) = do
  putStrLn "Tick"
  return (moveFirstBallForwardInGameStateAndUpdate game, p)
  
handleInput _ a = return a

stepGameState :: Float -> (GameState, Play) -> IO (GameState, Play)
stepGameState f (game, p) = return (newGame, p) where
  moveBalls = moveFirstBallForwardInGameStateAndUpdate game
  newGame = updateFreeBalls f moveBalls

