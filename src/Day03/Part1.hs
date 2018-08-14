-- Part1.hs
module Day03.Part1 where

import Day03

-- | solve the puzzle. Input is the cell we are looking for.
--
-- Note: Input is based/centered on square 1. Stream is (obviously)
-- indexed/based on index 0. This is why we need to pass on input - 1.
solve :: Int -> Int
solve input = distance $ (cells moves) !! (input - 1)
