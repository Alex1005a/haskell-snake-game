{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Domain where

import Relude
import Direction ( changeDirectionOrOld, Direction )
import Data.List.NonEmpty (  cons, singleton )
import Lens.Micro.TH (makeLenses)
import Coordinates (Coord, Bound, tryMoveCoord, X(..), Y(..))
import Data.List ( partition, (!!) )
import Relude.Extra.Lens ((%~), (.~), (^.),)
import System.Random (RandomGen)
import System.Random.Stateful (randomR)
import Relude.Extra (Lens')

newtype SnakeID = SnakeID Int
  deriving (Ord, Eq, Show)

data BaseSnake = BaseSnake { _sid :: SnakeID, _parts :: NonEmpty Coord }
  deriving (Eq)
makeLenses ''BaseSnake

class HasBaseSnake a where
  getBaseSnake :: a -> BaseSnake

class HasRandomGenState a where
  randomGen :: Lens' a Rand

data SnakeStatus
    = Valid
    | Crush

data NotValidatedSnake = NotValidatedSnake BaseSnake Direction

instance HasBaseSnake NotValidatedSnake where
  getBaseSnake (NotValidatedSnake b _) = b

data Snake (snakeStatus :: SnakeStatus) where
  ValidSnake :: BaseSnake -> Direction -> Snake 'Valid
  CrushSnake :: BaseSnake -> Snake 'Crush

instance HasBaseSnake (Snake a) where
  getBaseSnake ((ValidSnake b _)) = b
  getBaseSnake ((CrushSnake b)) = b

data SomeSnake = forall status. SomeSnake (Snake status)

instance HasBaseSnake SomeSnake where
  getBaseSnake (SomeSnake snake) = getBaseSnake snake

instance Eq SomeSnake where
  s1 == s2 = getBaseSnake s1 ^. sid == getBaseSnake s2 ^. sid
 
type Food = Coord

data Rand = forall g. RandomGen g => Rand g

data GameState = GameState {
    _foodCoord :: Food,
    _snakes :: NonEmpty SomeSnake,
    _rand :: Rand,
    _nextSid :: Int
  }
makeLenses ''GameState

instance HasRandomGenState GameState where
  randomGen = rand

instance HasRandomGenState Rand where
  randomGen = id

type GameMT m a = StateT GameState (ReaderT Bound m) a
type GameM a = GameMT Identity a

initTail :: NonEmpty a -> NonEmpty a
initTail l@(_ :| []) = l
initTail (x :| xs) = x :| fromMaybe [] (viaNonEmpty init xs)

allCoords :: Bound -> [Coord]
allCoords (X xBound, Y yBound)  = [(X x, Y y) | x <- [0..xBound], y <- [0..yBound]]

nextRandom :: (MonadState g m, HasRandomGenState g) => Int -> Int -> m Int
nextRandom min' max' = do
  (Rand gen) <- gets (^. randomGen)
  let (randNum, newGen) = randomR (min', max') gen
  modify $ randomGen .~ Rand newGen
  return randNum

generateFoodCoord' :: (MonadState g m, HasRandomGenState g) => [Coord] -> Bound -> m Food
generateFoodCoord' allSnakesParts bound = do
  let coordinates = filter (`notElem` allSnakesParts) $ allCoords bound
  let len = length coordinates
  randIndex <- nextRandom 0 (len - 1)
  return $ coordinates !! randIndex

generateFoodCoord :: [Coord] -> GameM ()
generateFoodCoord allSnakesParts = do
  bound <- ask
  newFoodCoord <- generateFoodCoord' allSnakesParts bound
  modify $ foodCoord .~ newFoodCoord

replace :: (Eq a) => a -> NonEmpty a -> NonEmpty a
replace replaceEl (x :| xs) =
  if x == replaceEl then replaceEl :| xs
  else x :| (replaceEl : filter (/= replaceEl) xs)

replaceSnake :: SomeSnake -> GameM ()
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

crushSnakesCoords :: GameM [Coord]
crushSnakesCoords = do
 allSnakes <- gets (^. snakes)
 return . snakesCoords $ allCrushSnakes allSnakes

tryMoveSnakeHead :: Bound -> NotValidatedSnake -> Either (Snake 'Crush) NotValidatedSnake
tryMoveSnakeHead bound (NotValidatedSnake baseSnake direction) = do
  let parts'@(headCoord :| _) = baseSnake ^. parts
  let movedHead = tryMoveCoord bound direction headCoord
  case movedHead of
    Just newHead -> do
      let newBase = baseSnake & parts .~ newHead :| toList parts'
      Right $ NotValidatedSnake newBase direction
    Nothing -> Left $ CrushSnake baseSnake

moveSnakes :: [NotValidatedSnake] -> GameM [NotValidatedSnake]
moveSnakes snakes' = do
  bound <- ask
  let movedSnakes = tryMoveSnakeHead bound <$> snakes'
  let crushedSnakes = [ SomeSnake crushIntoBound | Left crushIntoBound <- movedSnakes]
  let modifySnakePool = replaceSnake <$> crushedSnakes
  sequence_ modifySnakePool
  return $ [ snake | Right snake <- movedSnakes ]

snakesPopOrEatFood :: [NotValidatedSnake] -> GameM [NotValidatedSnake]
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
  unless (null eatingSnakes) $ generateFoodCoord $ snakesCoords newSnakes ++ crushCoords
  return newSnakes

checkSnakesCollisions :: NotValidatedSnake -> [NotValidatedSnake] -> [NotValidatedSnake] -> GameM [NotValidatedSnake]
checkSnakesCollisions currSnake@(NotValidatedSnake baseSnake _) oldSnakes newSnakes = do
  let (headSnake :| tailSnake) = baseSnake ^. parts
  crushCoords <- crushSnakesCoords
  let snakeCrashed = headSnake `elem` (tailSnake ++ snakesCoords (oldSnakes ++ newSnakes) ++ crushCoords)
  when snakeCrashed $ replaceSnake (SomeSnake (CrushSnake baseSnake))
  let newSnakes' = if snakeCrashed then newSnakes else currSnake : newSnakes
  case oldSnakes of
    [] -> return newSnakes'
    headOld : tailOld -> checkSnakesCollisions headOld tailOld newSnakes'

checkClashesBetweenSnakes :: [NotValidatedSnake] -> GameM [NotValidatedSnake]
checkClashesBetweenSnakes [] = return []
checkClashesBetweenSnakes (currSnake:tailSnakes) = checkSnakesCollisions currSnake tailSnakes []

changeSnakesDirection :: (Snake 'Valid -> Direction) -> Snake 'Valid -> NotValidatedSnake
changeSnakesDirection snakeToDir validSnake@(ValidSnake baseSnake direction) =
  NotValidatedSnake baseSnake $ changeDirectionOrOld (snakeToDir validSnake) direction

changeSnakesDirections :: [Snake 'Valid] -> (Snake 'Valid -> Direction) -> [NotValidatedSnake]
changeSnakesDirections  snakes' snakeToDir =
    changeSnakesDirection snakeToDir <$> snakes'

initGame :: (RandomGen g) => (NonEmpty Coord, Direction) -> Bound -> g -> (GameState, SnakeID)
initGame (parts', direction) bound gen = do
  let snakeID = SnakeID 1 -- Start with snake id 1
  let baseSnake = BaseSnake {
    _parts = parts',
    _sid = snakeID
  }
  let (food, newGen) = runState (generateFoodCoord' (toList parts') bound) (Rand gen)
  let gameState = GameState {
    _foodCoord = food,
    _snakes = singleton $ SomeSnake (ValidSnake baseSnake direction),
    _rand = newGen,
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

makeStep :: (Snake 'Valid -> Direction) -> GameM ()
makeStep snakeToDir = do
  allSnakes <- gets (^. snakes)
  let validSnakes = changeSnakesDirection snakeToDir <$> allValidSnakes allSnakes
  newSnakes <- validSnakes
    & moveSnakes
    >>= snakesPopOrEatFood
    >>= checkClashesBetweenSnakes
  let modifySnakePool =
        (\(NotValidatedSnake baseSnake dir) -> replaceSnake (SomeSnake (ValidSnake baseSnake dir)) )
        <$> newSnakes
  sequence_ modifySnakePool

execGameStep :: (Snake 'Valid -> Direction) -> GameState -> Bound -> GameState
execGameStep snakeToDir gameState = 
  runReader $ execStateT (makeStep snakeToDir) gameState