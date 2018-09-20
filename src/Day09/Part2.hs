-- Part2.hs
module Day09.Part2 where

import Control.Monad (foldM)
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)

import Day09

-- | solve the puzzle
solve :: String -> Int
solve input = getChars $ unsafePerformIO $ foldM processEvent (InGroup 0 (Stats 0 0)) input where
  getChars (InGroup 0 s) = numOfChars s
