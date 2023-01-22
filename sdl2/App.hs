{-# LANGUAGE TemplateHaskell #-}
module App where

import Relude
import Lens.Micro.TH ( makeLenses )
import Foreign.C (CInt)
import Snake
import Coordinates
import Linear (V4)
import SDL (Renderer)

data Config = Config {
  _partSize :: Int,
  _cPartSize :: CInt,
  _cHeight :: CInt,
  _cWidth :: CInt,
  _delayMillisecond :: Int,
  _snakesColours :: Map SnakeID (V4 Word8),
  _bound :: Bound,
  _renderer :: Renderer
} deriving (Show)
makeLenses ''Config
type App = ReaderT Config IO