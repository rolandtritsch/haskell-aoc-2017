module Main where

import Text.Printf

import Day01

main :: IO ()
main = do
  printf "Day01: Part1: Captcha -> %d\n" (Day01.captcha Day01.input)
  printf "Day01: Part2: Captcha' -> %d\n" (Day01.captcha' Day01.input)
