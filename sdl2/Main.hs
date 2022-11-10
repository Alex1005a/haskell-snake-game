{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SDL
import Domain
import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Prelude hiding (head, Left, Right)
import Data.Maybe (fromJust, isNothing)
import System.Random ( newStdGen )
import Data.List.NonEmpty (fromList)
import Data.Word ( Word8 )
import Control.Monad.State.Lazy ( evalStateT, evalState )
import Control.Lens ( (^.) )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Haskell Snake" $ defaultWindow { windowInitialSize = V2 (20*30) (18*30) }
  renderer <- createRenderer window (-1) defaultRenderer
  gen <- newStdGen
  let snake' = Snake (fromList [(0, 0)]) Right
  let world = evalState (initWorld snake' 17 19) gen
  appLoop renderer world
  destroyWindow window

changeDirection :: (Scancode -> Bool) ->  Maybe Direction
changeDirection isKeyPressed
  | isKeyPressed ScancodeW = Just Up
  | isKeyPressed ScancodeS = Just Down
  | isKeyPressed ScancodeA = Just Left
  | isKeyPressed ScancodeD = Just Right
  | otherwise = Nothing

drawCoord :: Renderer -> V4 Word8 -> Coord -> IO ()
drawCoord renderer v4 (x, y)  = do
  rendererDrawColor renderer $= v4
  fillRect renderer justRectangle
  where cx = fromIntegral (x*30)
        cy = fromIntegral (y*30)
        justRectangle = Just $ Rectangle (mkPoint cx cy) (V2 30 30)

drawGame :: Renderer -> World -> IO ()
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

appLoop :: Renderer -> World -> IO ()
appLoop renderer world = do
  pumpEvents
  isKeyPressed <- getKeyboardState
  let newDirection = changeDirection isKeyPressed

  let newWorldMaybe = evalStateT (updateWorld newDirection) world

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  let cHeight = (+) 1 $ fromIntegral $ world ^. height
  let cWidth = (+) 1 $ fromIntegral $ world ^. width
  drawRect renderer $ Just $ Rectangle (mkPoint 0 0) (V2 (cWidth*30) (cHeight*30))
  mapM_ (drawGame renderer) newWorldMaybe
  present renderer

  when (isNothing newWorldMaybe) $ drawGame renderer world >> present renderer >> waitKeyPress ScancodeEscape
  threadDelay $ 1000 * 90

  unless (isKeyPressed ScancodeEscape || isNothing newWorldMaybe) $ appLoop renderer (fromJust newWorldMaybe)

mkPoint :: a -> a -> Point V2 a
mkPoint x y = SDL.P $ V2 x y