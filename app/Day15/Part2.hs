module Day15.Part2 where

import Text.Printf (printf)
import System.TimeIt (timeItT)
import Control.Exception.Base (evaluate)

import Day15

-- | solve the puzzle
solve :: Int
solve = length $ filter (\(a, b) -> matching a b) $ take depth $ pairs where
  genConfigA = (703, 16807, 2147483647, 4)
  genConfigB = (516, 48271, 2147483647, 8)
  depth = 5000000
  pairs = zip (generator genConfigA) (generator genConfigB)

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate solve
  printf "Day15: Part2: count -> (%d, %f)\n" result time
