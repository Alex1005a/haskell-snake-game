{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Lens ( (^.), makeLenses )
import Foreign.C (CInt)
import Control.Monad.Reader (ReaderT (runReaderT), MonadTrans (lift), MonadReader (ask))



data Config = Config { 
  _partSize :: Int, 
  _cPartSize :: CInt, 
  _cHeight :: CInt,
  _cWidth :: CInt, 
  _delayMillisecond :: Int
} deriving (Show)
makeLenses ''Config

type Game = ReaderT Config IO

main :: IO ()
main = do
  initializeAll

  gen <- newStdGen
  let snake1 = Snake (singleton (0, 0)) Right
  let snake2 = Snake (singleton (1, 1)) Right
  let world = evalState (initWorld (snake1 :| [snake2]) 17 19) gen

  let config = Config {
      _partSize = 30,
      _cPartSize = 30,
      _cHeight = fromIntegral (world ^. height) + 1,
      _cWidth = fromIntegral (world ^. width) + 1,
      _delayMillisecond = 1000 * 90
    }

  window <- createWindow "Haskell Snake" $ defaultWindow {
    windowInitialSize = V2 (config ^. cWidth * config ^. cPartSize) (config ^. cHeight * config ^. cPartSize)
    }
  renderer <- createRenderer window (-1) defaultRenderer

  runReaderT (appLoop renderer world) config
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

drawCoord :: Renderer -> V4 Word8 -> Coord -> Game () --Config
drawCoord renderer v4 (x, y)  = do
  config <- ask
  let cx = fromIntegral $ x * (config ^. partSize)
  let cy = fromIntegral $ y * (config ^. partSize)
  let justRectangle = Just $ Rectangle (mkPoint cx cy) (V2 (config ^. cPartSize) (config ^. cPartSize))

  rendererDrawColor renderer $= v4
  fillRect renderer justRectangle

drawSnake :: Renderer -> V4 Word8 -> Snake -> Game ()
drawSnake renderer v4 snake =
  mapM_ (drawCoord renderer v4) $ snake ^. parts

drawGame :: Renderer -> World -> Game ()
drawGame renderer world = do
  drawCoord renderer (V4 255 0 0 255) (world ^. foodCoord)
  let snakes' = toList $ world ^. snakes
  mapM_ (\(snake, v4) -> drawSnake renderer v4 snake) $ zip snakes' [V4 0 214 120 255, V4 0 0 255 255]

waitQuit :: Scancode -> IO ()
waitQuit scancode = do
  event <- waitEvent
  case eventPayload event of
   KeyboardEvent (KeyboardEventData _ _ _ (Keysym scancode' _ _)) ->
    if scancode == scancode'
    then return () else waitQuit scancode
   QuitEvent -> return ()
   _ -> waitQuit scancode

appLoop :: Renderer -> World -> Game ()
appLoop renderer world = do
  config <- ask
  pumpEvents
  isKeyPressed <- getKeyboardState
  
  let newDirection1 = changeDirection1 isKeyPressed
  let newDirection2 = changeDirection2 isKeyPressed
  let newWorldMaybe = evalStateT (tryUpdateWorld (newDirection1 :| [newDirection2])) world

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255

  drawRect renderer $ Just $ Rectangle (mkPoint 0 0) (V2 (config ^. cWidth * config ^. cPartSize) (config ^. cHeight * config ^. cPartSize))
  mapM_ (drawGame renderer) newWorldMaybe
  present renderer

  when (isNothing newWorldMaybe) $ drawGame renderer world >> present renderer >> lift (waitQuit ScancodeEscape)

  lift $ threadDelay $ config ^. delayMillisecond
  isQuit <- any (\ev -> eventPayload ev == QuitEvent) <$> pollEvents
  unless (isKeyPressed ScancodeEscape || isNothing newWorldMaybe || isQuit) $ appLoop renderer (fromJust newWorldMaybe)

mkPoint :: a -> a -> Point V2 a
mkPoint x y = SDL.P $ V2 x y