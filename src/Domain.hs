{-# LANGUAGE TemplateHaskell #-}
module Domain ( foodCoord, height, initGameState, parts, snake, updateGameState,
      width, Coord, Direction(..), GameState, Snake(Snake) ) where
import Data.List.NonEmpty (NonEmpty ((:|)), toList, head, tail, fromList, init)
import Prelude hiding (init, tail, head, Left, Right)
import System.Random ( Random(randomR), StdGen )
import Control.Lens ( (^.), makeLenses, (.~), (&) )
import Control.Monad.Morph ( generalize, MFunctor(hoist), MonadTrans(lift) )
import Control.Monad.State.Lazy ( MonadState(put, get), StateT, State )


data Direction = Up | Down | Left | Right deriving (Eq, Show)

type Coord = (Int, Int)

data Snake = Snake { _parts :: NonEmpty Coord, _direction :: Direction } deriving (Show)

data GameState = GameState { _foodCoord :: Coord, _height :: Int, _width :: Int, _snake :: Snake } deriving (Show)

makeLenses ''Snake
makeLenses ''GameState

shiftPosition ::  Direction -> (Int, Int)
shiftPosition dir =
  case dir of
  Up -> (0, -1 )
  Down -> (0, 1 )
  Left  -> (-1 , 0)
  Right -> (1 , 0)

pop :: NonEmpty a -> NonEmpty a
pop xs = fromList $ init xs

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x + u, y + v)

headSnake :: Snake -> Coord
headSnake snake' = head $ snake' ^. parts

snakeNotCrush :: Snake -> Maybe Snake
snakeNotCrush snake'
    | headSnake snake' `elem` tailSnake = Nothing
    | otherwise = Just snake'
    where tailSnake = tail $ snake' ^. parts

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

generateFoodCoord :: Snake -> Int -> Int -> State StdGen Coord
generateFoodCoord snake' height' width' = do
  let coordinates = filter (\coord -> coord `notElem` (snake' ^. parts) ) $ allCoords height' width'
  let len = length coordinates
  randIndex <- nextRandom 0 (len - 1)
  return $ coordinates !! randIndex

snakeInBound :: Snake -> Int -> Int -> Maybe Snake
snakeInBound snake' height' width' = if headSnake snake' `elem` coordinates then Just snake' else Nothing
  where coordinates = allCoords height' width'

addNewHeadSnake :: Snake -> Int -> Int -> Maybe Snake
addNewHeadSnake snake' height' width' = do
  let notVerifiedSnake = snake' & parts .~ newPos :| toList (snake' ^. parts)
  snakeInBound notVerifiedSnake height' width' >>= snakeNotCrush
  where newPos = add (headSnake snake') $ shiftPosition $ snake' ^. direction

initGameState :: Snake -> Int -> Int -> State StdGen GameState
initGameState snake' height' width' = do
  coord <- generateFoodCoord snake' height' width'
  return GameState { _foodCoord = coord, _height = height', _width = width', _snake = snake'}

updateGameStateFoodCoord :: GameState -> Snake -> State StdGen GameState
updateGameStateFoodCoord gameState newSnake = do
  coord <- generateFoodCoord newSnake (gameState ^. height) (gameState ^. width)
  return $ gameState & snake .~ newSnake 
                     & foodCoord .~ coord

updateGameState ::  Maybe Direction -> GameState -> StateT StdGen Maybe GameState
updateGameState maybeDirection gameState = do
  let oldDirection = gameState ^. snake . direction
  let newDirection = maybe oldDirection (`validateDirection` oldDirection) maybeDirection
  let snakeNewDirection = gameState ^. snake & direction .~ newDirection
  newHeadSnake <- lift $ addNewHeadSnake snakeNewDirection (gameState ^. height) (gameState ^. width)
  if gameState ^. foodCoord == headSnake newHeadSnake then do
    hoist generalize $ updateGameStateFoodCoord gameState newHeadSnake
  else do 
    let newSnake = newHeadSnake & parts .~ pop (newHeadSnake ^. parts)
    return $ gameState & snake .~ newSnake