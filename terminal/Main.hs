module Main (main) where

import Domain ( foodCoord, initGameState, parts, snake, updateGameState, Coord, Direction(..), GameState, Snake(Snake) )
import Terminal.Game
    ( assertTermDims, (%), box, cell,
      centerFull, errorPress,playGame, blankPlane, (&), Game(Game),
      Event(..), Coords, Height, Plane, Width, StdGen, Draw )
import qualified Data.Tuple as T
import Prelude hiding (Left, Right)
import Data.List.NonEmpty (fromList)
import Control.Monad.State (runState, StateT (runStateT))
import System.Random (newStdGen)
import Data.Maybe (isNothing, fromJust)
import Control.Lens ((^.))

main :: IO ()
main = do
      gen <- newStdGen
      let snake' = Snake (fromList [(0, 0)]) Right
      let (gameState, gen') = runState (initGameState snake' mh mw) gen
      sizeCheck
      errorPress $ playGame $ aloneInARoom gameState gen'
      where
          (mh, mw) = snd boundaries


-- game specification
aloneInARoom :: GameState -> StdGen -> Game MyState
aloneInARoom gameState gen = Game 13                       -- ticks per second
                    (MyState (10, 10) gameState gen
                             Stop False)     -- init state
                    (\_ s e -> logicFun s e) -- logic function
                    (\r s -> centerFull r $
                               drawFun s)    -- draw function
                    gsQuit                   -- quit function

sizeCheck :: IO ()
sizeCheck = let (w, h) = T.swap . snd $ boundaries
            in assertTermDims w h

-------------------------------------------------------------------------------
-- Types

data MyState = MyState { gsCoord :: Coords,
                         gsGameState :: GameState,
                         stdGen :: StdGen,
                         gsMove  :: Move,
                         gsQuit  :: Bool }
             deriving (Show)

data Move = N | S | E | W | Stop
          deriving (Show, Eq)

boundaries :: (Coords, Coords)
boundaries = ((1, 1), (20, 40))

-------------------------------------------------------------------------------
-- Logic

logicFun :: MyState -> Event -> MyState
logicFun gs (KeyPress 'q') = gs { gsQuit = True }
logicFun gs Tick = do
      let result = runStateT (updateGameState Nothing (gsGameState gs)) (stdGen gs)
      if isNothing result then gs { gsQuit = True }
      else gs { gsGameState =  fst (fromJust result), stdGen = snd (fromJust result) }
logicFun gs (KeyPress c)   = do
      let result = runStateT (updateGameState (changeDirection c) (gsGameState gs)) (stdGen gs)
      if isNothing result then gs { gsQuit = True }
      else gs { gsGameState =  fst (fromJust result), stdGen = snd (fromJust result) }


changeDirection :: Char ->  Maybe Direction
changeDirection 'w' = Just Up
changeDirection 's' = Just Down
changeDirection 'a' = Just Left
changeDirection 'd' = Just Right
changeDirection _ = Nothing



-------------------------------------------------------------------------------
-- Draw

swapAdd1 ::  (Int, Int) -> (Int, Int)
swapAdd1 (x, y) = (y + 1, x + 1)

drawCoord :: Char -> Coord -> Draw
drawCoord ch coord = coord % cell ch

drawSnake :: Snake -> Plane -> Plane
drawSnake snake' plane = do
      foldr (\a b -> b & drawCoord '*' a) plane $ swapAdd1 <$> snake' ^. parts

drawFun :: MyState -> Plane
drawFun (MyState _ gameState _ _ _) =
            blankPlane mw     mh            &
                (1, 1)   % box mw mh '.'                   &
                (2, 2)   % box (mw-2) (mh-2) ' '           &
                drawSnake (gameState ^. snake)             &
                drawCoord 'A' (swapAdd1 (gameState ^. foodCoord)) 
    where
          mh :: Height
          mw :: Width
          (mh, mw) = snd boundaries