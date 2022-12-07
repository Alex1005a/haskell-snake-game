{-# LANGUAGE TemplateHaskell #-}
module Domain ( foodCoord, height, initWorld, parts, snakes, tryUpdateWorld,
      width, Coord, Direction(..), World, Snake(Snake) ) where
import Data.List.NonEmpty (NonEmpty ((:|)), toList, head, tail, fromList, init, zip, filter)
import Prelude hiding (filter, zip, init, tail, head, Left, Right)
import System.Random ( Random(randomR), StdGen )
import Lens.Micro ( (^.), (.~), (&), (%~), _1, _2)
import Lens.Micro.TH ( makeLenses )
import Control.Monad.Morph ( generalize, MFunctor(hoist), MonadTrans(lift) )
import Control.Monad.State.Lazy ( MonadState(put, get), StateT, State )
import Control.Monad.State (runState)

data Direction = Up | Down | Left | Right deriving (Eq, Show)

type Coord = (Int, Int)

data Snake = Snake { _parts :: NonEmpty Coord, _direction :: Direction } deriving (Show, Eq)

data World = World {
  _foodCoord :: Coord,
  _height :: Int,
  _width :: Int,
  _snakes :: NonEmpty Snake,
  _stdGen :: StdGen
  } deriving (Show)

makeLenses ''Snake
makeLenses ''World

shiftPosition :: Direction -> (Int, Int)
shiftPosition dir =
  case dir of
  Up -> (0, -1 )
  Down -> (0, 1 )
  Left  -> (-1 , 0)
  Right -> (1 , 0)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:_) = Just a

pop :: NonEmpty a -> NonEmpty a
pop xs = fromList $ init xs

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x + u, y + v)

headSnake :: Snake -> Coord
headSnake snake' = head $ snake' ^. parts

snakeNotCrush :: NonEmpty Snake -> Snake -> Maybe Snake
snakeNotCrush snakes' snake
    | headSnake snake `elem` tailSnakes = Nothing
    | otherwise = Just snake
    where tailSnakes = foldr (\snake' part -> tail (snake' ^. parts) <> part) [] snakes'

validateDirection :: Direction -> Direction -> Direction
validateDirection newDirection oldDirection
  | newRightOldLeft || newLeftOldRight || newUpOldDown || newDownOldUp = oldDirection
  | otherwise = newDirection
  where newRightOldLeft = newDirection == Right && oldDirection == Left
        newLeftOldRight = newDirection == Left && oldDirection == Right
        newUpOldDown = newDirection == Up && oldDirection == Down
        newDownOldUp = newDirection == Down && oldDirection == Up

allCoords :: Int -> Int -> [Coord]
allCoords height' width'  = [(x, y) | x <- [0..width'], y <- [0..height']]

nextRandom :: Int -> Int -> State StdGen Int
nextRandom min' max' = do
  gen <- get
  let (randIndex, gen') = randomR (min', max') gen
  put gen'
  return randIndex

generateFoodCoord :: NonEmpty Snake -> Int -> Int -> State StdGen Coord
generateFoodCoord snakes' height' width' = do
  let allSnakesParts = foldr (\snake part -> toList (snake ^. parts) <> part) [] snakes'
  let coordinates = filter (`notElem` allSnakesParts) $ fromList $ allCoords height' width'
  let len = length coordinates
  randIndex <- nextRandom 0 (len - 1)
  return $ coordinates !! randIndex

snakeInBound :: Snake -> Int -> Int -> Maybe Snake
snakeInBound snake height' width' 
  | headSnake' ^. _1 < 0 || headSnake' ^. _1 > width' 
    || headSnake' ^. _2 < 0 || headSnake' ^. _2 > height' = Nothing
  | otherwise = Just snake
  where headSnake' = headSnake snake

tryAddNewHeadSnake :: Snake -> World -> Maybe Snake
tryAddNewHeadSnake snake world = do
  let notVerifiedSnake = snake & parts .~ newPos :| toList (snake ^. parts)
  snakeInBound notVerifiedSnake (world ^. height) (world ^. width) >>= snakeNotCrush (world ^. snakes)
  where newPos = add (headSnake snake) $ shiftPosition $ snake ^. direction

initWorld :: NonEmpty Snake -> Int -> Int -> State StdGen World
initWorld snakes' height' width' = do
  coord <- generateFoodCoord snakes' height' width'
  gen <- get
  return World {
          _foodCoord = coord,
          _height = height',
          _width = width',
          _snakes = snakes',
          _stdGen = gen
        }

updateWorldFoodCoord :: NonEmpty Snake -> State World World
updateWorldFoodCoord newSnakes = do
  world <- get
  let (coord, gen) = runState (generateFoodCoord newSnakes (world ^. height) (world ^. width)) (world ^. stdGen)
  return $ world & snakes .~ newSnakes
                 & foodCoord .~ coord
                 & stdGen .~ gen

tryUpdateWorld :: NonEmpty (Maybe Direction) -> StateT World Maybe World
tryUpdateWorld maybeDirections = do
  world <- get
  let oldDirections = (^. direction) <$> (world ^. snakes)
  let newDirections = (\(oldDirection, maybeDirection)
                        -> maybe oldDirection (`validateDirection` oldDirection) maybeDirection)
                      <$> zip oldDirections maybeDirections
  let snakesNewDirections = uncurry (direction .~)
                      <$> zip newDirections (world ^. snakes)
  newHeadSnakes <- lift $ mapM (`tryAddNewHeadSnake` world) snakesNewDirections
  let snakeEatenFood =  safeHead $ filter (\snake -> (world ^. foodCoord) == headSnake snake) newHeadSnakes
  case snakeEatenFood of
    Just snake -> do
      --let otherSnakes = (parts %~ pop) <$> filter (/= snake) newHeadSnakes
      hoist generalize $ updateWorldFoodCoord $ 
        (\snake' -> if snake' /= snake  then snake' & parts %~ pop else snake) <$> newHeadSnakes
    Nothing -> do
      let newSnakes = (parts %~ pop) <$> newHeadSnakes
      return $ world & snakes .~ newSnakes
  
  
