{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Snake where

import Relude
import Direction ( Direction )
import Lens.Micro.TH (makeLenses)
import Coordinates
import Relude.Extra.Lens ((^.))

-- A unique ID that remains with the snake regardless of its state
newtype SnakeID = SnakeID Int
  deriving (Ord, Eq, Show)

-- Record that all snakes have
data BaseSnake = BaseSnake { _sid :: SnakeID, _parts :: NonEmpty Coord }
  deriving (Eq)
makeLenses ''BaseSnake

class HasBaseSnake a where
  getBaseSnake :: a -> BaseSnake

data SnakeStatus
    = Valid
    | Crush

-- Intermediate state of the snake when moving it and checking for collisions
data NotValidatedSnake = NotValidatedSnake BaseSnake Direction

instance HasBaseSnake NotValidatedSnake where
  getBaseSnake :: NotValidatedSnake -> BaseSnake
  getBaseSnake (NotValidatedSnake b _) = b

{-
ValidSnake - a snake that can move and eat food.
CrushSnake - А snake colliding with its tail or with another snake or with a border. Can't move or eat food.
ValidSnake has a direction in which it moves.
-}
data Snake (snakeStatus :: SnakeStatus) where
  ValidSnake :: BaseSnake -> Direction -> Snake 'Valid
  CrushSnake :: BaseSnake -> Snake 'Crush

instance HasBaseSnake (Snake a) where
  getBaseSnake ((ValidSnake b _)) = b
  getBaseSnake ((CrushSnake b)) = b

data SomeSnake = forall status. SomeSnake (Snake status)

instance HasBaseSnake SomeSnake where
  getBaseSnake (SomeSnake snake) = getBaseSnake snake

-- Snakes compare by SnakeID
instance Eq SomeSnake where
  s1 == s2 = getBaseSnake s1 ^. sid == getBaseSnake s2 ^. sid