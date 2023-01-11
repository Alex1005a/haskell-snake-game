module Coordinates (Coord, Bound, tryMoveCoord, X(..), Y(..)) where

import Relude
import Direction

naturalPlusInt :: Natural -> Int -> Maybe Natural
naturalPlusInt nat int = do
  let naturalFromInt = integerToNatural . toInteger
  naturalFromInt $ int + fromEnum nat

newtype X = X Natural deriving (Eq, Ord, Show)

xPlusInt :: X -> Int -> Maybe X
xPlusInt (X nat) int = X <$> naturalPlusInt nat int

newtype Y = Y Natural deriving (Eq, Ord, Show)

yPlusInt :: Y -> Int -> Maybe Y
yPlusInt (Y nat) int = Y <$> naturalPlusInt nat int

{--
Y in coordinate system inverted
0---1---2 - X 
|       |
1       |
|       |
2-------Bound
|
Y
--}
type Coord = (X, Y)
type Bound = Coord

shiftTuple :: Direction -> (Int, Int)
shiftTuple North = (0, -1)
shiftTuple South = (0, 1)
shiftTuple West  = (-1, 0)
shiftTuple East  = (1, 0)

-- Moves the coordinate in the given direction if it does not go out of bounds
tryMoveCoord :: Bound -> Direction -> Coord -> Maybe Coord
tryMoveCoord (xBound, yBound) direction (xOld, yOld) = do
  let (xShift, yShift) = shiftTuple direction
  xNew <- xPlusInt xOld xShift
  yNew <- yPlusInt yOld yShift
  if xNew > xBound || yNew > yBound 
    then Nothing
    else Just (xNew, yNew)