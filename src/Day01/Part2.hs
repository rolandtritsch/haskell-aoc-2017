module Day01.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | solve the puzzle
solve :: String -> Int
solve number = captcha' number

-- | main
main :: IO ()
main = do
  (d02p1t, d02p1r) <- timeItT $ evaluate (solve input)
  printf "Day02: Part1: captcha' -> (%f, %d)\n" d02p1t d02p1r
