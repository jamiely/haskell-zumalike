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
    (fakeGame2, Play)
    drawGame
    handleInput
    stepGame where
      size = (500, 500)
      position = (0, 0)

drawGame :: (Game, Play) -> IO Picture
{-drawGame (game, _) = return (grid <> plays)-}
drawGame (game, _) = return (grid <> gamePicture) where
  grid = 
    color black (line [ (-100, -300), (-100,  300) ]) <>
    color black (line [ ( 100, -300), ( 100,  300) ]) <>
    color black (line [ (-300,  100), ( 300,  100) ]) <>
    color black (line [ (-300, -100), ( 300, -100) ]) <>
    color blue (circle 10)
  
  (Game _ transits) = game
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
drawPosition (Position (Ball _ width) (IndexedPoint _ (Point x y))) = 
  translate x y $ color red $ circle width

handleInput :: Event -> (Game, Play) -> IO (Game, Play)
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y))
  (game, p) = do
    let newGame = game -- TODO: add ball to game in first transit
    return (newGame, p)
    {-let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) .-}
           {-(+ 50)-}
    {-(gridX, gridY) = (snap x, snap y)-}
    {-case (game !! gridX) !! gridY of-}
      {-Just _ -> return (game, X)-}
      {-Nothing -> do-}
        {-let newGame = (ix gridX . ix gridY .~ (Just X)) game-}
        {-return (newGame, O)-}
handleInput _ a = return a

stepGame :: Float -> (Game, Play) -> IO (Game, Play)
stepGame _ = return

