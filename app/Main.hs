{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Prelude hiding (head, Left, Right)
import Data.Maybe (fromJust, isNothing)
import System.Random ( newStdGen, StdGen )
import Data.List.NonEmpty (fromList)
import Data.Word ( Word8 )
import Control.Monad.State.Lazy ( runState,StateT(runStateT) )
import Control.Lens ( (^.) )
import Domain
    ( foodCoord, height, initGameState, parts, snake, updateGameState,
      width, Coord, Direction(..), GameState, Snake(Snake) )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" $ defaultWindow { windowInitialSize = V2 (20*30) (18*30) }
  renderer <- createRenderer window (-1) defaultRenderer
  gen <- newStdGen
  let snake' = Snake (fromList [(0, 0)]) Right
  let (gameState, gen') = runState (initGameState snake' 17 19) gen
  appLoop renderer gen' gameState
  destroyWindow window

changeDirection :: (Scancode -> Bool) -> IO (Maybe Direction)
changeDirection isKeyPressed
  | isKeyPressed ScancodeW = return $ Just Up
  | isKeyPressed ScancodeS = return $ Just Down
  | isKeyPressed ScancodeA = return $ Just Left
  | isKeyPressed ScancodeD = return $ Just Right
  | otherwise = return Nothing

drawCoord :: Renderer -> V4 Word8 -> Coord -> IO ()
drawCoord renderer v4 (x, y)  = do
  rendererDrawColor renderer $= v4
  fillRect renderer justRectangle
  where cx = fromIntegral (x*30)
        cy = fromIntegral (y*30)
        justRectangle = Just $ Rectangle (mkPoint cx cy) (V2 30 30)

drawGame :: Renderer -> GameState -> IO ()
drawGame renderer gameState = do
  drawCoord renderer (V4 255 0 0 255) (gameState ^. foodCoord)
  mapM_ (drawCoord renderer (V4 0 214 120 255)) $ gameState ^. snake . parts

waitKeyPress :: Scancode -> IO ()
waitKeyPress scancode = do
  event <- waitEvent
  case eventPayload event of
   KeyboardEvent (KeyboardEventData _ _ _ (Keysym scancode' _ _)) ->
    if scancode == scancode'
    then return () else waitKeyPress scancode
   _ -> waitKeyPress scancode

appLoop :: Renderer -> StdGen -> GameState -> IO ()
appLoop renderer gen gameState = do
  pumpEvents
  isKeyPressed <- getKeyboardState
  newDirection <- changeDirection isKeyPressed

  let newGameStateMaybe = runStateT (updateGameState newDirection gameState) gen

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  let cHeight = (+) 1 $ fromIntegral $ gameState ^. height
  let cWidth = (+) 1 $ fromIntegral $ gameState ^. width
  drawRect renderer $ Just $ Rectangle (mkPoint 0 0) (V2 (cWidth*30) (cHeight*30))
  mapM_ (\(state', _) -> drawGame renderer state') newGameStateMaybe
  present renderer

  when (isNothing newGameStateMaybe) $ drawGame renderer gameState >> present renderer >> waitKeyPress ScancodeEscape
  threadDelay $ 1000 * 90

  unless (isKeyPressed ScancodeEscape || isNothing newGameStateMaybe) $ appLoop renderer (snd (fromJust newGameStateMaybe)) (fst (fromJust newGameStateMaybe))

mkPoint :: a -> a -> Point V2 a
mkPoint x y = SDL.P $ V2 x y