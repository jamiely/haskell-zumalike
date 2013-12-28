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
      size = (1400, 800)
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
  
  (GameState _ transits) = game
  gamePicture = foldr (<>) Blank $ map drawTransit transits

  {-plays = mconcat-}
    {-[ translate (fromIntegral $ (x - 1) * 200)-}
                {-(fromIntegral $ (y - 1) * 200) $-}
        {-case p of-}
          {-Play -> color white (thickCircle 1 50)-}
    {-| x <- [0..2]-}
    {-, y <- [0..2]-}
    {-, Just p <- [ (game !! x) !! y ]-}
    {-]-}

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
    putStrLn $ show game
    let newGameState = game -- TODO: add ball to game in first transit
    return (newGameState, p)
    {-let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) .-}
           {-(+ 50)-}
    {-(gridX, gridY) = (snap x, snap y)-}
    {-case (game !! gridX) !! gridY of-}
      {-Just _ -> return (game, X)-}
      {-Nothing -> do-}
        {-let newGameState = (ix gridX . ix gridY .~ (Just X)) game-}
        {-return (newGameState, O)-}
handleInput _ a = return a

stepGameState :: Float -> (GameState, Play) -> IO (GameState, Play)
stepGameState _ = return

