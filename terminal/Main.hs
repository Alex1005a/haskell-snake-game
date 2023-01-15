{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Domain
import Snake
import Direction
import Coordinates
import GameState
import Relude
import Relude.Extra.Lens ((^.), (.~))
import Lens.Micro.TH ( makeLenses )
import Terminal.Game
    ( assertTermDims, (%), box, cell,
      centerFull, errorPress,playGame, blankPlane, Game(Game),
      Event(..), Coords, Height, Plane, Width, Draw )
import Data.List.NonEmpty (singleton)
import System.Random (newStdGen)

data SnakeGameState = SnakeGameState { 
                                       _gameState :: GameState,
                                       _bound :: Bound,
                                       _quit  :: Bool 
                                     }
makeLenses ''SnakeGameState

main :: IO ()
main = do
      gen <- newStdGen
      print "Press Enter to start"
      _ <- getLine
      let snake = (singleton (X 0, Y 0), East)
      let coordShift = 3
      let (height, width) = (fromIntegral $ mh - coordShift, fromIntegral $ mw - coordShift)
      let bound' = (X width, Y height)
      let (gameState', _) = initGame snake bound' gen

      sizeCheck
      errorPress $ playGame $ createSnakeGame gameState' bound'
      where
          (mh, mw) = boundaries

createSnakeGame :: GameState -> Bound -> Game SnakeGameState
createSnakeGame gameState' bound' = Game 9                  
                    (SnakeGameState gameState' bound' False)  
                    (\_ s e -> logic s e) 
                    (\r s -> centerFull r $
                               draw s)    
                    _quit                   

sizeCheck :: IO ()
sizeCheck = let (w, h) = swap boundaries
            in assertTermDims w h

boundaries :: Coords
boundaries = (20, 40)

logic :: SnakeGameState -> Event -> SnakeGameState
logic gs (KeyPress 'q') = gs & quit .~ True
logic gs (KeyPress '\ESC') = gs & quit .~ True
logic gs Tick = do
      let newGameState = 
            execGameStep (\(ValidSnake _ direction) -> direction)
            (gs ^. gameState)
            (gs ^. bound)
      gs & gameState .~ newGameState
logic gs (KeyPress c) = do
      let newGameState = 
            execGameStep (\(ValidSnake _ direction) -> fromMaybe direction $ changeDirection c)
            (gs ^. gameState) 
            (gs ^. bound)
      gs & gameState .~ newGameState

changeDirection :: Char ->  Maybe Direction
changeDirection 'w' = Just North
changeDirection 's' = Just South
changeDirection 'a' = Just West
changeDirection 'd' = Just East
changeDirection _ = Nothing

swapAdd2 ::  Coord -> (Int, Int)
swapAdd2 (X x, Y y) = (fromIntegral y + 2, fromIntegral x + 2)

drawCoord :: Char -> (Int, Int) -> Draw
drawCoord ch coord = coord % cell ch

drawSnake :: SomeSnake -> Plane -> Plane
drawSnake snake' plane = do
      foldr (\a b -> b & drawCoord '*' a) plane $ swapAdd2 <$> getBaseSnake snake' ^. parts

draw :: SnakeGameState -> Plane
draw (SnakeGameState gs _ _) =
            blankPlane mw mh                               &
                (1, 1)   % box mw mh '#'                   &
                (2, 2)   % box (mw-2) (mh-2) ' '           &
                drawSnake (head (gs ^. snakes))         &
                drawCoord 'A' (swapAdd2 (gs ^. foodCoord)) 
    where
          mh :: Height
          mw :: Width
          (mh, mw) = boundaries
