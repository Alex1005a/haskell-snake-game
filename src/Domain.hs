{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain where

import Relude
import Snake
import GameState
import NonEmptyExt
import Direction ( changeDirectionOrOld, Direction )
import Data.List.NonEmpty (  cons, singleton )
import Coordinates (Coord, Bound, tryMoveCoord, X(..), Y(..))
import Data.List ( partition, (!!) )
import Relude.Extra.Lens ((%~), (.~), (^.),)
import System.Random (RandomGen)
import System.Random.Stateful (randomR)
import Control.Monad.Except (throwError)
import Control.Monad.Error (catchError)

-- All possible unique coordinates within the given bounds
allCoords :: Bound -> [Coord]
allCoords (X xBound, Y yBound) = [(X x, Y y) | x <- [0..xBound], y <- [0..yBound]]

nextRandom :: (MonadState g m, HasRandomGenState g) => Int -> Int -> m Int
nextRandom min' max' = do
  (Rand gen) <- gets (^. randomGen)
  let (randNum, newGen) = randomR (min', max') gen
  modify $ randomGen .~ Rand newGen
  return randNum

-- Generates a food coordinate that is not in the given list
generateFoodCoord :: (MonadState g m, HasRandomGenState g, BoundReader b m) => [Coord] -> m Food
generateFoodCoord allSnakesParts = do
  bound <- asks getBound
  let coordinates = filter (`notElem` allSnakesParts) $ allCoords bound
  let len = length coordinates
  randIndex <- nextRandom 0 (len - 1)
  return $ coordinates !! randIndex

-- Changees the food coordinate with a new, generated generateFoodCoord
changeFoodCoord :: (MonadState g m, HasRandomGenState g, HasFoodState g, BoundReader b m) => [Coord] -> m ()
changeFoodCoord allSnakesParts = do
  newFoodCoord <- generateFoodCoord allSnakesParts
  modify $ foodCoord .~ newFoodCoord

-- Replaces a snake with the same SnakeID with this snake
replaceSnake :: (MonadState s m, HasSnakePoolState s) => SomeSnake -> m ()
replaceSnake snake =
  modify $ snakes %~ replace snake

snakesCoords :: (Foldable list, HasBaseSnake snake) => list snake -> [Coord]
snakesCoords =
  foldr (\snake acc
    -> acc ++ toList (getBaseSnake snake ^. parts)) []

allCrushSnakes :: NonEmpty SomeSnake -> [Snake 'Crush]
allCrushSnakes snakes' = do
  [ crushSnake | (SomeSnake crushSnake@CrushSnake {}) <- toList snakes' ]

allValidSnakes :: NonEmpty SomeSnake -> [Snake 'Valid]
allValidSnakes snakes' = do
    [ validSnake | (SomeSnake validSnake@ValidSnake {}) <- toList  snakes' ]

crushSnakesCoords :: (MonadState s m, HasSnakePoolState s) => m [Coord]
crushSnakesCoords = do
 allSnakes <- gets (^. snakes)
 return . snakesCoords $ allCrushSnakes allSnakes

{-
Move snake head moving in that direction.
If moving successfully return snake with new head, old parts as tail.
If new head out of bound, return crush snake.
-}
tryMoveSnakeHead :: (BoundReader b m) => NotValidatedSnake -> ExceptT (Snake 'Crush) m NotValidatedSnake
tryMoveSnakeHead (NotValidatedSnake baseSnake direction) = do
  bound <- asks getBound
  let parts'@(headCoord :| _) = baseSnake ^. parts
  let movedHead = tryMoveCoord bound direction headCoord
  case movedHead of
    Just newHead -> do
      let newBase = baseSnake & parts .~ newHead :| toList parts'
      return $ NotValidatedSnake newBase direction
    Nothing -> throwError $ CrushSnake baseSnake
    
-- Apply tryMoveSnakeHead for all snakes in list and return those not crushed 
moveSnakes :: (MonadState s m, HasSnakePoolState s, BoundReader b m) => [NotValidatedSnake] -> m [NotValidatedSnake]
moveSnakes snakes' = do
  (crushSnakes, movedSnakes) <- partitionEithers <$> mapM (runExceptT . tryMoveSnakeHead) snakes'
  mapM_ (replaceSnake . SomeSnake) crushSnakes
  return movedSnakes

{-
For those snakes whose head is not on food, remove the last element in the tail.
If there is a snake whose head is on the food, then the coordinates of the food change.
-}
snakesPopOrEatFood :: (MonadGame s b m) => [NotValidatedSnake] -> m [NotValidatedSnake]
snakesPopOrEatFood snakes' = do
  food <- gets (^. foodCoord)
  let (eatingSnakes, notEatingSnakes) =
       partition (\(NotValidatedSnake baseSnake _)
          -> head (baseSnake ^. parts) == food) snakes'
  let newNotEatSnakes =
        (\(NotValidatedSnake baseSnake direction)
          -> NotValidatedSnake (baseSnake & parts %~ initTail) direction)
        <$> notEatingSnakes
  let newSnakes = newNotEatSnakes ++ eatingSnakes
  crushCoords <- crushSnakesCoords
  unless (null eatingSnakes) $ changeFoodCoord $ snakesCoords newSnakes ++ crushCoords
  return newSnakes

{-
First argument - checked snake, second - rest snakes which will be checked, 
third - new snakes that return at the end (initialize empty list).
For snake check collisions with her tail, crush snakes and snakes in two lists.
If snake crush, replace it in pool and continue.
Else add in in new snakes list.
Return new list when will it end rest snakes
-}
checkSnakesCollisions :: (MonadState s m, HasSnakePoolState s) =>
  NotValidatedSnake -> [NotValidatedSnake] -> [NotValidatedSnake] -> m [NotValidatedSnake]
checkSnakesCollisions currSnake@(NotValidatedSnake baseSnake _) oldSnakes newSnakes = do
  let (headSnake :| tailSnake) = baseSnake ^. parts
  crushCoords <- crushSnakesCoords
  let snakeCrashed = headSnake `elem` (tailSnake ++ snakesCoords (oldSnakes ++ newSnakes) ++ crushCoords)
  when snakeCrashed $ replaceSnake (SomeSnake (CrushSnake baseSnake))
  let newSnakes' = if snakeCrashed then newSnakes else currSnake : newSnakes
  case oldSnakes of
    [] -> return newSnakes'
    headOld : tailOld -> checkSnakesCollisions headOld tailOld newSnakes'

checkClashesBetweenSnakes :: (MonadState s m, HasSnakePoolState s) => [NotValidatedSnake] -> m [NotValidatedSnake]
checkClashesBetweenSnakes [] = return []
checkClashesBetweenSnakes (currSnake:tailSnakes) = checkSnakesCollisions currSnake tailSnakes []

changeSnakesDirection :: (Snake 'Valid -> Direction) -> Snake 'Valid -> NotValidatedSnake
changeSnakesDirection snakeToDir validSnake@(ValidSnake baseSnake direction) =
  NotValidatedSnake baseSnake $ changeDirectionOrOld (snakeToDir validSnake) direction

changeSnakesDirections :: [Snake 'Valid] -> (Snake 'Valid -> Direction) -> [NotValidatedSnake]
changeSnakesDirections  snakes' snakeToDir =
    changeSnakesDirection snakeToDir <$> snakes'

-- Create game state with first snake
initGame :: (RandomGen g) => (NonEmpty Coord, Direction) -> Bound -> g -> (GameState, SnakeID)
initGame (parts', direction) bound gen = do
  let snakeID = SnakeID 1 -- Start with snake id 1
  let baseSnake = BaseSnake {
    _parts = parts',
    _sid = snakeID
  }
  let (food, newRand) = runReader (runStateT (generateFoodCoord (toList parts')) (Rand gen)) bound
  let gameState = GameState {
    _foodCoord' = food,
    _snakePool = singleton $ SomeSnake (ValidSnake baseSnake direction),
    _rand = newRand,
    _nextSid = 2 -- next snake id = 2
  }
  (gameState, snakeID)

addSnakeToGame :: (NonEmpty Coord, Direction) -> State GameState SnakeID
addSnakeToGame (parts', direction)  = do
  nextSid' <- gets (^. nextSid)
  let snakeID = SnakeID nextSid'
  let baseSnake = BaseSnake {
    _parts = parts',
    _sid = snakeID
  }
  modify (\gameState ->
        gameState & snakes %~ cons (SomeSnake (ValidSnake baseSnake direction))
                  & nextSid %~ (+ 1))
  return snakeID

makeStep :: (MonadGame s b m) => (Snake 'Valid -> Direction) -> m ()
makeStep getNewDirection = do
  allSnakes <- gets (^. snakes)
  let validSnakes = changeSnakesDirection getNewDirection <$> allValidSnakes allSnakes
  newSnakes <- validSnakes
    & moveSnakes
    >>= snakesPopOrEatFood
    >>= checkClashesBetweenSnakes
  let modifySnakePool =
        (\(NotValidatedSnake baseSnake dir) -> replaceSnake (SomeSnake (ValidSnake baseSnake dir)) )
        <$> newSnakes
  sequence_ modifySnakePool

execGameStep :: (Snake 'Valid -> Direction) -> GameState -> Bound -> GameState
execGameStep getNewDirection gameState =
  runReader $ execStateT (makeStep getNewDirection) gameState