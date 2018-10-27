module Day01.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: String -> Int
solve number = captcha number

-- | main
main :: IO ()
main = do
  (d01p1t, d01p1r) <- timeItT $ evaluate (solve input)
  printf "Day01: Part1: captcha -> (%f, %d)\n" d01p1t d01p1r
