-- Part1.hs
module Day09.Part1 where

import Control.Monad (foldM)
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)

import Day09

-- | solve the puzzle
solve :: String -> Int
solve input = getScore $ unsafePerformIO $ foldM processEvent (InGroup 0 (Stats 0 0)) input where
  getScore (InGroup 0 s) = score s

-- | decorate the FSM with logging
withLogging :: (Show s, Show e) => FSM s e -> FSM s e
withLogging fsm s e = do
  nextState <- fsm s e
  printf "%s -> %s -> %s\n" (show s) (show e) (show nextState)
  return nextState

-- | solve the puzzle (debug version)
solve' :: String -> Int
solve' input = getScore $ unsafePerformIO $ foldM (withLogging processEvent) (InGroup 0 (Stats 0 0)) input where
  getScore (InGroup 0 s) = score s
