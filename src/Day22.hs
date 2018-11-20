{-|
Problem: <https://adventofcode.com/2017/day/22>

Solution:

General - Basicly I am implementing two grids (with a static/
pre-allocated size (not good; needs to be fixed)) and walk
these grids follwoing the rules in the problem statement.

Part1 - Walk the [[SimpleGrid]] and count the infections.

Part2 - Walk the [[AdvancedGrid]] and count the infections.
-}
module Day22 where

import Prelude hiding (Left, Right)

--import Data.Array

import Util

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

data Position = Position Int Int deriving (Show, Eq)

data NodeState
  = Clean
  | Infected
  | Weakend
  | Flagged
  deriving (Show, Eq)

type Grid = [[NodeState]]

data GridState = GridState Grid Position Direction Int deriving (Show, Eq)

-- | read the input.
input :: GridState
input = GridState grid (midpoint grid) Up 0 where
  grid = (map processLine . inputRaw) "input/Day22input.txt" where
    processLine l = map processNode l where
      processNode '.' = Clean
      processNode '#' = Infected
      processNode 'W' = Weakend
      processNode 'F' = Flagged
      processNode _ = error "Unknown state char detected."
  midpoint g = Position mp mp where
    mp = div (length g) 2

-- | turn left.
turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Down = Right
turnLeft Left = Down
turnLeft Right = Up

-- | turn right.
turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Down = Left
turnRight Left = Up
turnRight Right = Down

-- | check state of the node.
inState :: Grid -> Position -> NodeState -> Bool
inState ss (Position row col) s = (ss !! row) !! col == s

-- | update the state grid with a new state.
update :: Position -> NodeState -> Grid -> Grid
update (Position r c) s ss =  headRows ++ [newRow] ++ tailRows where
  headRows = take r ss
  tailRows = drop (r + 1) ss
  oldRow = ss !! r
  newRow = headCols ++ [s] ++ tailCols where
    headCols = take c oldRow
    tailCols = drop (c + 1) oldRow

-- | do a burst.
burst :: GridState -> GridState
burst (GridState g p d i)
  | inState g p Infected = GridState g p (turnRight d) i
  | inState g p Clean = GridState (update p Infected g) p (turnLeft d) (i + 1)
  | otherwise = GridState (update p Clean g) p (turnLeft d) i

-- | do one move/step forward.
move :: GridState -> GridState
move (GridState g (Position row col) Up i) = GridState g (Position (row - 1) col) Up i
move (GridState g (Position row col) Down i) = GridState g (Position (row + 1) col) Down i
move (GridState g (Position row col) Left i) = GridState g (Position  row (col - 1)) Left i
move (GridState g (Position row col) Right i) = GridState g (Position row (col + 1)) Right i

-- | run the simulation for a number of bursts.
runSimulation :: Int -> GridState -> GridState
runSimulation 0 g = g
runSimulation bursts g = runSimulation (bursts - 1) (move $ burst g)
