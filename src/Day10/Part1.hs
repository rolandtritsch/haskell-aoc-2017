-- Part1.hs
module Day10.Part1 where

import Day10

-- | solve the puzzle
solve :: [Length] -> Int
solve lengths = foldl (*) 1 $ take 2 $ knot lengths
