{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Networks where
import Relude
import Network.WebSockets (WebSocketsData (..), DataMessage (..), runServer)
import Data.Serialize (Serialize, encodeLazy, decodeLazy)
import Snake (SnakeID(..), BaseSnake)
import Direction (Direction)
import Coordinates
import GameState (Food)
import Control.Concurrent (Chan, forkIO, readChan, ThreadId)
import Network.WebSockets.Connection (Connection)
import Network.WebSockets (sendBinaryData)
import Network.WebSockets (receiveData)
import Network.WebSockets.Client (runClient)
import Network.Socket (withSocketsDo)
import Network.WebSockets (acceptRequest)

data StartMessage = StartMessage deriving Generic

data GameRender = GameRender {
  _snakes' :: [BaseSnake],
  _food :: Food
} deriving (Generic)

instance Serialize StartMessage
instance Serialize SnakeID
instance Serialize Direction
instance Serialize X
instance Serialize Y
instance Serialize (NonEmpty Coord)
instance Serialize BaseSnake
instance Serialize GameRender

decodeErrorMessage :: Text
decodeErrorMessage = fromString "Invalid websocket message"

instance (Serialize s) => WebSocketsData s where
  fromLazyByteString byteStr = do
    case decodeLazy byteStr of
      Right decodeObj -> decodeObj
      Left _ -> error decodeErrorMessage
  toLazyByteString = encodeLazy
  fromDataMessage (Binary bs) = fromLazyByteString bs
  fromDataMessage _ = error decodeErrorMessage

startClient :: MVar SnakeID -> Chan Direction -> MVar GameRender -> Connection -> IO ()
startClient snakeIdVar directionChan renderChan conn = do
  sendBinaryData conn StartMessage
  sid' :: SnakeID <- receiveData conn
  putMVar snakeIdVar sid'
  _ <- forkIO $ sendDataWithModify (sid', ) conn directionChan
  recvData conn renderChan

startClientAsync :: String -> Int -> Chan Direction -> MVar GameRender -> IO SnakeID
startClientAsync ip port directionChan renderChan  = do
  snakeIdVar <- newEmptyMVar
  void $ forkIO 
    $ withSocketsDo $ runClient ip port "/" 
    $ startClient snakeIdVar directionChan renderChan
  takeMVar snakeIdVar

startServer :: MVar () -> SnakeID -> MVar (SnakeID, Direction) -> Chan GameRender -> Connection -> IO ()
startServer startVar sid' directionChan renderChan conn = do
  _ :: StartMessage <- receiveData conn
  putMVar startVar ()
  sendBinaryData conn sid'
  _ <- forkIO $ sendData conn renderChan
  recvData conn directionChan

startServerAsync :: String -> Int -> SnakeID -> MVar (SnakeID, Direction) -> Chan GameRender -> IO ThreadId
startServerAsync ip port sid' directionChan renderChan = do
  startVar <- newEmptyMVar
  threadId <- forkIO 
    $ runServer ip port 
    $ startServer startVar sid' directionChan renderChan <=< acceptRequest
  takeMVar startVar >> return threadId

sendDataWithModify :: (WebSocketsData b) => (a -> b) -> Connection -> Chan a -> IO ()
sendDataWithModify modifyMessage conn ch  = do
  forever $ do
    message <- readChan ch
    sendBinaryData conn $ modifyMessage message

recvData :: (WebSocketsData a) => Connection -> MVar a  -> IO ()
recvData conn var = do
  forever $ do
    message <- receiveData conn
    putMVar var message

sendData :: (WebSocketsData a) => Connection -> Chan a -> IO ()
sendData = sendDataWithModify id