{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SDL
import Domain
import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Prelude hiding (head, Left, Right)
import Data.Maybe (fromJust, isNothing)
import System.Random ( newStdGen )
import Data.List.NonEmpty (NonEmpty ((:|)), singleton, toList)
import Data.Word ( Word8 )
import Control.Monad.State.Lazy ( evalStateT, evalState )
import Control.Lens ( (^.) )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Haskell Snake" $ defaultWindow { windowInitialSize = V2 (20*30) (18*30) }
  renderer <- createRenderer window (-1) defaultRenderer
  gen <- newStdGen
  let snake1 = Snake (singleton (0, 0)) Right
  let snake2 = Snake (singleton (1, 1)) Right
  let world = evalState (initWorld (snake1 :| [snake2]) 17 19) gen
  appLoop renderer world
  destroyWindow window

changeDirection1 :: (Scancode -> Bool) ->  Maybe Direction
changeDirection1 isKeyPressed
  | isKeyPressed ScancodeW = Just Up
  | isKeyPressed ScancodeS = Just Down
  | isKeyPressed ScancodeA = Just Left
  | isKeyPressed ScancodeD = Just Right
  | otherwise = Nothing

changeDirection2 :: (Scancode -> Bool) ->  Maybe Direction
changeDirection2 isKeyPressed
  | isKeyPressed ScancodeUp = Just Up
  | isKeyPressed ScancodeDown = Just Down
  | isKeyPressed ScancodeLeft = Just Left
  | isKeyPressed ScancodeRight = Just Right
  | otherwise = Nothing

drawCoord :: Renderer -> V4 Word8 -> Coord -> IO ()
drawCoord renderer v4 (x, y)  = do
  rendererDrawColor renderer $= v4
  fillRect renderer justRectangle
  where cx = fromIntegral (x*30)
        cy = fromIntegral (y*30)
        justRectangle = Just $ Rectangle (mkPoint cx cy) (V2 30 30)

drawSnake :: Renderer -> V4 Word8 -> Snake -> IO ()
drawSnake renderer v4 snake =
  mapM_ (drawCoord renderer v4) $ snake ^. parts

drawGame :: Renderer -> World -> IO ()
drawGame renderer world = do
  drawCoord renderer (V4 255 0 0 255) (world ^. foodCoord)
  let snakes' = toList $ world ^. snakes
  --drawSnake renderer (V4 0 214 120 255) (snakes' !! 0)
  --drawSnake renderer (V4 0 0 255 255) (snakes' !! 1)
  mapM_ (\(snake, v4) -> drawSnake renderer v4 snake) $ zip snakes' [V4 0 214 120 255, V4 0 0 255 255]

  --mapM_ (drawCoord renderer (V4 0 214 120 255)) $ foldr (\snake part -> toList (snake ^. parts) <>  part) [] (world ^. snakes)

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

  let newDirection1 = changeDirection1 isKeyPressed
  let newDirection2 = changeDirection2 isKeyPressed
  let newWorldMaybe = evalStateT (tryUpdateWorld (newDirection1 :| [newDirection2])) world

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