module Days.Day3 where

import           Prelude hiding (Left, Right)

day3 :: IO ()
day3 = do
  let part1Input = 361527
  let answer = manhattanDistance $ spiralCoords !! (part1Input - 1)
  print answer

type Coord = (Int, Int)

data Direction
  = Right
  | Left
  | Down
  | Up
  deriving (Show, Eq)

origin :: Coord
origin = (0, 0)

move :: Coord -> Direction -> Coord
move (x, y) dir = case dir of
  Right -> (x + 1, y)
  Left  -> (x - 1, y)
  Down  -> (x, y - 1)
  Up    -> (x, y + 1)

spiralMovements :: [Direction]
spiralMovements = do
  (direction, count) <- zipped
  replicate count direction
  where
    directions = cycle [Right, Up, Left, Down]
    twice = replicate 2 =<< [1..]
    zipped = zip directions twice

spiralCoords :: [Coord]
spiralCoords = scanl move origin spiralMovements

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y
