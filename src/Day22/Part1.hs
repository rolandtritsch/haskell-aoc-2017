module Day22.Part1 where

import Text.Printf (printf)
import System.TimeIt (timeItT)
import Control.Exception.Base (evaluate)

import Day22

-- | solve the puzzle
solve :: GridState -> Int
solve grid = numberOfNodesThatGotInfected where
  (GridState _ _ _ numberOfNodesThatGotInfected) = runSimulation 10000 grid

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day22: Part1: simple -> (%f, %d)\n" time result
