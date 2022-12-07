{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude hiding (head, Left, Right)
import Domain 
import Terminal.Game
    ( assertTermDims, (%), box, cell,
      centerFull, errorPress,playGame, blankPlane, (&), Game(Game),
      Event(..), Coords, Height, Plane, Width, Draw )
import qualified Data.Tuple as T
import Prelude hiding (Left, Right)
import Data.List.NonEmpty (singleton, NonEmpty ((:|)), head)
import Control.Monad.State ( evalState, evalStateT )
import System.Random (newStdGen)
import Lens.Micro ((^.), (.~))
import Lens.Micro.TH ( makeLenses )

data SnakeGameState = SnakeGameState { 
                                       _gsWorld :: World,
                                       _gsQuit  :: Bool 
                                     }
             deriving (Show)
makeLenses ''SnakeGameState

main :: IO ()
main = do
      gen <- newStdGen
      _ <- getLine
      let snake' = Snake (singleton (0, 0)) Right
      let world = evalState (initWorld (snake' :| []) (mh-3) (mw-3)) gen
      sizeCheck
      errorPress $ playGame $ createSnakeGame world
      where
          (mh, mw) = boundaries

createSnakeGame :: World -> Game SnakeGameState
createSnakeGame world = Game 9                  
                    (SnakeGameState world False)  
                    (\_ s e -> logicFun s e) 
                    (\r s -> centerFull r $
                               drawFun s)    
                    _gsQuit                   

sizeCheck :: IO ()
sizeCheck = let (w, h) = T.swap boundaries
            in assertTermDims w h

boundaries :: Coords
boundaries = (20, 40)

logicFun :: SnakeGameState -> Event -> SnakeGameState
logicFun gs (KeyPress 'q') = gs { _gsQuit = True }
logicFun gs (KeyPress '\ESC') = gs { _gsQuit = True }
logicFun gs Tick = do
      let newWorldMaybe = evalStateT (tryUpdateWorld (singleton Nothing)) (gs ^. gsWorld)
      maybe (gs & gsQuit .~ True) (\world -> gs & gsWorld .~ world) newWorldMaybe
logicFun gs (KeyPress c)   = do
      let newWorldMaybe = evalStateT (tryUpdateWorld (singleton(changeDirection c))) (_gsWorld gs)
      maybe (gs & gsQuit .~ True) (\world -> gs & gsWorld .~ world) newWorldMaybe

changeDirection :: Char ->  Maybe Direction
changeDirection 'w' = Just Up
changeDirection 's' = Just Down
changeDirection 'a' = Just Left
changeDirection 'd' = Just Right
changeDirection _ = Nothing

swapAdd2 ::  (Int, Int) -> (Int, Int)
swapAdd2 (x, y) = (y + 2, x + 2)

drawCoord :: Char -> Coord -> Draw
drawCoord ch coord = coord % cell ch

drawSnake :: Snake -> Plane -> Plane
drawSnake snake' plane = do
      foldr (\a b -> b & drawCoord '*' a) plane $ swapAdd2 <$> snake' ^. parts

drawFun :: SnakeGameState -> Plane
drawFun (SnakeGameState world _) =
            blankPlane mw mh                               &
                (1, 1)   % box mw mh '#'                   &
                (2, 2)   % box (mw-2) (mh-2) ' '           &
                drawSnake (Data.List.NonEmpty.head (world ^. snakes))         &
                drawCoord 'A' (swapAdd2 (world ^. foodCoord)) 
    where
          mh :: Height
          mw :: Width
          (mh, mw) = boundaries
