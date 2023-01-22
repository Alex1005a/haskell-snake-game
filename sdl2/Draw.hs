module Draw where

import SDL
import Relude
import Snake
import GameState
import Coordinates
import Relude.Extra.Lens ( (^.))
import Data.Maybe ( fromJust )
import Data.Map.Lazy (lookup)
import App

drawCoord :: Renderer -> V4 Word8 -> Coord -> App ()
drawCoord renderer' v4 (X x, Y y)  = do
  config <- ask
  let cx = fromIntegral $ fromInteger (toInteger x) * config ^. partSize
  let cy = fromIntegral $ fromInteger (toInteger y) * config ^. partSize
  let justRectangle = Just $ Rectangle (mkPoint cx cy) $ V2 (config ^. cPartSize) (config ^. cPartSize)

  rendererDrawColor renderer' $= v4
  fillRect renderer' justRectangle

drawSnake :: (HasBaseSnake snake) => Renderer -> V4 Word8 -> snake -> App ()
drawSnake renderer' v4 snake =
  mapM_ (drawCoord renderer' v4) $ getBaseSnake snake ^. parts

drawGame :: (HasBaseSnake snake) => Renderer -> Food -> [snake] -> Map SnakeID (V4 Word8) -> App ()
drawGame renderer' food snakes' sidToV4' = do
  let snakesWithV4 = (\snake -> (snake , fromJust $ lookup (getBaseSnake snake ^. sid) sidToV4')) <$> snakes'
  mapM_ (\(snake, v4) -> drawSnake renderer' v4 snake) snakesWithV4
  drawCoord renderer' (V4 255 0 0 255) food

drawPresent :: (HasBaseSnake snake) =>  Food -> [snake] -> App ()
drawPresent food' snakes' = do
  config <- ask
  let renderer' = config ^. renderer
  rendererDrawColor renderer' $= V4 0 0 0 255
  clear renderer'
  rendererDrawColor renderer' $= V4 255 255 255 255

  drawRect renderer'
    $ Just $ Rectangle (mkPoint 0 0) 
    $ V2 (config ^. cWidth * config ^. cPartSize) (config ^. cHeight * config ^. cPartSize)
  drawGame renderer' food' snakes' $ config ^. snakesColours
  
  present renderer'

mkPoint :: a -> a -> Point V2 a
mkPoint x y = P $ V2 x y

loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture renderer' path = do
  bmp <- loadBMP path
  createTextureFromSurface renderer' bmp <* freeSurface bmp

renderTexture ::  Texture -> App ()
renderTexture tex = do
  renderer' <- asks (^. renderer)
  ti <- queryTexture tex
  let (w, h) = (textureWidth ti, textureHeight ti)
      pos'   = mkPoint 0 0
      extent = V2 w h
  copy renderer' tex Nothing $ Just $ Rectangle pos' extent