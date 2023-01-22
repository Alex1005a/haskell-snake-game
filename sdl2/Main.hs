{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Concurrent
    ( threadDelay, writeChan, newChan )
import System.Random ( newStdGen )
import Data.List.NonEmpty (singleton)
import Data.Map.Lazy (findWithDefault, lookup, insert)
import Control.Concurrent.Chan (Chan)
import Networks
import App
import Draw
import Paths_haskell_snake (getDataFileName)

data GameMode = Single | Two | Server | Client

main :: IO ()
main = do
  initializeAll
  gen <- newStdGen

  let (ip, port) = ("127.0.0.1", 3000) --for test
  let (height, width) = (17, 30)
  let (cHeight', cWidth') = (fromIntegral height + 1, fromIntegral width + 1)
  let partSize' = 30

  let snake1 = (singleton (X 0, Y 0), East)
  let snake2 = (singleton (X width , Y height), West)

  let bound' = (X width, Y height)
  let (gameState, snakeId1) = initGame snake1 bound' gen
  let (snakeId2, gameStateTwoSnakes) = runState (addSnakeToGame snake2) gameState

  window <- createWindow "Haskell Snake" $ defaultWindow {
    windowInitialSize = V2 (cWidth' * partSize') (cHeight' * partSize')
    }
  renderer' <- createRenderer window (-1) defaultRenderer
  
  let config = Config {
      _partSize = fromIntegral partSize',
      _cPartSize = partSize',
      _cHeight = cHeight',
      _cWidth = cWidth',
      _delayMillisecond = 1000 * 90,
      _snakesColours = fromList [(snakeId1, V4 0 214 120 255)],
      _bound = bound',
      _renderer = renderer'
    }
  let snake2Colour = V4 0 0 255 255
  let configTwoSnakes = config & snakesColours %~ insert snakeId2 snake2Colour

  screen <- getDataFileName "assets/screen.bmp" >>= loadTexture renderer'
  runReaderT (renderTexture screen) config
  present renderer'

  selectedMode <- waitKeysDown 
    $ fromList [(ScancodeR, Single), (ScancodeT, Two), (ScancodeN, Server), (ScancodeM, Client)]
  destroyTexture screen

  case selectedMode of
    Just Single -> runReaderT (appLoop gameState) config
    Just Two -> runReaderT (appLoop gameStateTwoSnakes) configTwoSnakes
    Just Server -> do
      directionChan <- newChan
      renderChan <- newEmptyMVar
      void $ startServerAsync ip port snakeId2 renderChan directionChan
      runReaderT (serverLoop renderChan directionChan gameStateTwoSnakes) configTwoSnakes
    Just Client -> do
      directionChan <- newEmptyMVar
      renderChan <- newChan
      snakeId2' <- startClientAsync ip port renderChan directionChan
      runReaderT (clientLoop renderChan directionChan) 
        $ config & snakesColours %~ insert snakeId2' snake2Colour
    Nothing -> return ()

  destroyWindow window

changeDirection :: (Scancode, Scancode, Scancode, Scancode) -> (Scancode -> Bool) -> Maybe Direction
changeDirection (up, down, left, right) isKeyPressed
  | isKeyPressed up =    Just North
  | isKeyPressed down =  Just South
  | isKeyPressed left =  Just West
  | isKeyPressed right = Just East
  | otherwise = Nothing

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

delayAndCheckQuit :: (Scancode -> Bool) -> App () -> App ()
delayAndCheckQuit isKeyPressed nextStep = do
  delayTime <- asks (^. delayMillisecond)
  lift $ threadDelay delayTime
  isQuit <- any (\ev -> eventPayload ev == QuitEvent) <$> pollEvents
  unless (isKeyPressed ScancodeEscape || isQuit) nextStep

clientLoop :: Chan Direction -> MVar GameRender -> App ()
clientLoop directionChan renderChan = do
  gameRender <- lift $ takeMVar renderChan

  isKeyPressed <- getKeyboardState
  let newDirectionMaybe = changeDirection (ScancodeW, ScancodeS, ScancodeA, ScancodeD) isKeyPressed
  forM_ newDirectionMaybe $ lift . writeChan directionChan

  drawPresent (_food gameRender) (_snakes' gameRender)

  delayAndCheckQuit isKeyPressed $ clientLoop directionChan renderChan

serverLoop :: MVar (SnakeID, Direction) -> Chan GameRender -> GameState -> App ()
serverLoop directionChan renderChan gameState = do
  config <- ask
  isKeyPressed <- getKeyboardState
  let newDirection1 = changeDirection (ScancodeW, ScancodeS, ScancodeA, ScancodeD) isKeyPressed
  newDirection2 <- tryTakeMVar directionChan

  let sids = keys $ config ^. snakesColours

  let sidToDirection :: Map SnakeID Direction =
        fromList $ catMaybes [liftA2 (,) (sids !!? 0) newDirection1, newDirection2]
  let newGameState =
        execGameStep (\(ValidSnake baseSnake direction) -> findWithDefault direction (baseSnake ^. sid) sidToDirection)
        gameState $ config ^. bound
  
  let gameRender = GameRender (getBaseSnake <$> toList (newGameState ^. snakes)) $ newGameState ^. foodCoord
  lift $ writeChan renderChan gameRender

  drawPresent (newGameState ^. foodCoord) (toList $ newGameState ^. snakes) 

  delayAndCheckQuit isKeyPressed $ serverLoop directionChan renderChan newGameState

appLoop :: GameState -> App ()
appLoop gameState = do
  config <- ask
  isKeyPressed <- getKeyboardState

  let changeDirection' =  (`changeDirection` isKeyPressed)
  let newDirection1 = changeDirection' (ScancodeW, ScancodeS, ScancodeA, ScancodeD)
  let newDirection2 = changeDirection' (ScancodeUp, ScancodeDown, ScancodeLeft, ScancodeRight)

  let sids = keys $ config ^. snakesColours

  let sidToDirection :: Map SnakeID Direction =
        fromList $ catMaybes [liftA2 (,) (sids !!? 0) newDirection1, liftA2 (,) (sids !!? 1) newDirection2]
  let newGameState =
        execGameStep (\(ValidSnake baseSnake direction) -> findWithDefault direction (baseSnake ^. sid) sidToDirection)
        gameState $ config ^. bound
        
  drawPresent (newGameState ^. foodCoord) (toList $ newGameState ^. snakes) 

  delayAndCheckQuit isKeyPressed $ appLoop newGameState