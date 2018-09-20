-- Part1.hs
module Day09.Part1 where

import Day09

-- | solve the puzzle
solve :: String -> Int
solve input = getScore $ foldl transition (InGroup 0 (Stats 0 0)) input where
  getScore (InGroup 0 s) = score s
