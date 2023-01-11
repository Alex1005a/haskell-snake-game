module Direction (Direction(..), changeDirectionOrOld) where

import Relude hiding (Left, Right)

data Direction = North | South | West | East deriving (Eq, Show)

-- If directions are opposite, return second argument, else return first
changeDirectionOrOld :: Direction -> Direction -> Direction
changeDirectionOrOld newDirection oldDirection
  | newRightOldLeft || newLeftOldRight || newUpOldDown || newDownOldUp = oldDirection
  | otherwise = newDirection
  where newRightOldLeft = newDirection == East && oldDirection == West
        newLeftOldRight = newDirection == West && oldDirection == East
        newUpOldDown = newDirection == North && oldDirection == South
        newDownOldUp = newDirection == South && oldDirection == North
