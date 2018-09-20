-- Part2.hs
module Day09.Part2 where

import Day09

-- | solve the puzzle
solve :: String -> Int
solve input = getChars $ foldl transition (InGroup 0 (Stats 0 0)) input where
  getChars (InGroup 0 s) = numOfChars s
