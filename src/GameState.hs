{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module GameState where

import Relude
import Snake
import Lens.Micro.TH (makeLenses)
import Coordinates (Coord, Bound)
import System.Random (RandomGen)
import Relude.Extra (Lens')

class HasRandomGenState a where
  randomGen :: Lens' a Rand

class HasFoodState a where
  foodCoord :: Lens' a Food

class HasSnakePoolState a where
  snakes :: Lens' a (NonEmpty SomeSnake)

class HasBound a where
  getBound :: a -> Bound

type BoundReader b m = (MonadReader b m, HasBound b)
type GeneralState s m = (MonadState s m, HasRandomGenState s, HasFoodState s, HasSnakePoolState s)
type MonadGame s b m = (GeneralState s m, BoundReader b m)
 
type Food = Coord

data Rand = forall g. RandomGen g => Rand g

data GameState = GameState {
    _foodCoord' :: Food,
    _snakePool :: NonEmpty SomeSnake,
    _rand :: Rand,
    _nextSid :: Int
  }
makeLenses ''GameState

instance HasRandomGenState GameState where
  randomGen = rand

instance HasFoodState GameState where
  foodCoord = foodCoord'

instance HasSnakePoolState GameState where
  snakes = snakePool

instance HasBound Bound where
  getBound = id

instance HasRandomGenState Rand where
  randomGen = id