{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import SDL
import Relude
import Domain
import Snake
import GameState
import Coordinates
import Direction ( Direction (..) )
import Relude.Extra.Lens ( (^.), (%~))
import Relude.Extra (keys)
import Control.Concurrent (threadDelay)
import Data.Maybe ( fromJust )
import System.Random ( newStdGen )
import Data.List.NonEmpty (singleton)
import Lens.Micro.TH ( makeLenses )
import Foreign.C (CInt)
import Data.Map.Lazy (findWithDefault, lookup, insert)

data Config = Config {
  _partSize :: Int,
  _cPartSize :: CInt,
  _cHeight :: CInt,
  _cWidth :: CInt,
  _delayMillisecond :: Int,
  _snakesColours :: Map SnakeID (V4 Word8),
  _bound :: Bound
} deriving (Show)
makeLenses ''Config

type App = ReaderT Config IO

data GameMode = Single | Two

main :: IO ()
main = do
  initializeAll
  gen <- newStdGen

  let (height, width) = (17, 30)
  let snake1 = (singleton (X 0, Y 0), East)
  let snake2 = (singleton (X width , Y height), West)
  
  let bound' = (X width, Y height)
  let (gameState, snakeId1) = initGame snake1 bound' gen

  let config = Config {
      _partSize = 30,
      _cPartSize = 30,
      _cHeight = fromIntegral height + 1,
      _cWidth = fromIntegral width + 1,
      _delayMillisecond = 1000 * 90,
      _snakesColours = fromList [(snakeId1, V4 0 214 120 255)],
      _bound = bound'
    }

  window <- createWindow "Haskell Snake" $ defaultWindow {
    windowInitialSize = V2 (config ^. cWidth * config ^. cPartSize) (config ^. cHeight * config ^. cPartSize)
    }
  renderer <- createRenderer window (-1) defaultRenderer
  
 
  selectedMode <- waitKeysDown $ fromList [(ScancodeR, Single), (ScancodeT, Two)]
  
  case selectedMode of
    Just Single -> runReaderT (appLoop renderer gameState) config
    Just Two -> do
      let (snakeId2, startGameState) = runState (addSnakeToGame snake2) gameState
      runReaderT (appLoop renderer startGameState) $ config & snakesColours %~ insert snakeId2 (V4 0 0 255 255)
    Nothing -> return ()
  
  destroyWindow window


changeDirection :: (Scancode, Scancode, Scancode, Scancode) -> (Scancode -> Bool) -> Maybe Direction
changeDirection (up, down, left, right) isKeyPressed
  | isKeyPressed up =    Just North
  | isKeyPressed down =  Just South
  | isKeyPressed left =  Just West
  | isKeyPressed right = Just East
  | otherwise = Nothing

drawCoord :: Renderer -> V4 Word8 -> Coord -> App ()
drawCoord renderer v4 (X x, Y y)  = do
  config <- ask
  let cx = fromIntegral $ fromInteger (toInteger x) * config ^. partSize
  let cy = fromIntegral $ fromInteger (toInteger y) * config ^. partSize
  let justRectangle = Just $ Rectangle (mkPoint cx cy) $ V2 (config ^. cPartSize) (config ^. cPartSize)

  rendererDrawColor renderer $= v4
  fillRect renderer justRectangle

drawSnake :: Renderer -> V4 Word8 -> SomeSnake -> App ()
drawSnake renderer v4 snake =
  mapM_ (drawCoord renderer v4) $ getBaseSnake snake ^. parts

drawGame :: Renderer -> GameState -> Map SnakeID (V4 Word8) -> App ()
drawGame renderer gameState sidToV4' = do
  let snakes' = (\snake -> (snake , fromJust $ lookup (getBaseSnake snake ^. sid) sidToV4')) <$> toList (gameState ^. snakes)
  mapM_ (\(snake, v4) -> drawSnake renderer v4 snake) snakes'
  drawCoord renderer (V4 255 0 0 255) (gameState ^. foodCoord)

waitKeysDown :: Map Scancode a -> IO (Maybe a)
waitKeysDown scancodes = do
  event <- waitEvent
  case eventPayload event of
   KeyboardEvent (KeyboardEventData _ _ _ (Keysym scancode _ _)) ->
    case lookup scancode scancodes of
      Just value -> return $ Just value
      Nothing -> waitKeysDown scancodes
   QuitEvent -> return Nothing
   _ -> waitKeysDown scancodes

appLoop :: Renderer -> GameState -> App ()
appLoop renderer gameState = do
  config <- ask
  pumpEvents
  isKeyPressed <- getKeyboardState

  let changeDirection' =  (`changeDirection` isKeyPressed)
  let newDirection1 = changeDirection' (ScancodeW, ScancodeS, ScancodeA, ScancodeD)
  let newDirection2 = changeDirection' (ScancodeUp, ScancodeDown, ScancodeLeft, ScancodeRight)

  let sids = keys (config ^. snakesColours)

  let sidToDirection :: Map SnakeID Direction = 
        fromList $ catMaybes [liftA2 (,) (sids !!? 0) newDirection1, liftA2 (,) (sids !!? 1) newDirection2]
  let newGameState =
        execGameStep (\(ValidSnake baseSnake direction) -> findWithDefault direction (baseSnake ^. sid) sidToDirection)
        gameState (config ^. bound)

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255

  drawRect renderer $ Just $ Rectangle (mkPoint 0 0) $ V2 (config ^. cWidth * config ^. cPartSize) (config ^. cHeight * config ^. cPartSize)
  drawGame renderer newGameState $ config ^. snakesColours
  present renderer

  lift $ threadDelay $ config ^. delayMillisecond
  isQuit <- any (\ev -> eventPayload ev == QuitEvent) <$> pollEvents
  unless (isKeyPressed ScancodeEscape || isQuit) $ appLoop renderer newGameState

mkPoint :: a -> a -> Point V2 a
mkPoint x y = SDL.P $ V2 x y
