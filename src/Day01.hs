-- Day01.hs
module Day01 where

import Data.Char (digitToInt)
import System.IO.Unsafe

input :: String
input = do
  let contents = unsafePerformIO $ readFile "input/Day01input.txt"
  take (length contents - 1) contents

-- |`captcha` returns the captcha for the given number.
-- Algorithm goes like this ...
-- * turn the string into a list of ints
-- * append the first element of the list to the end of the list
-- * move a sliding window of two elements over the list
-- * add up all windows where the elements are the same
captcha :: String -> Int
captcha s = go (prep s) where
  prep s = do
    let digits = map digitToInt s
    digits ++ [head digits]
  go (d : []) = 0
  go (d : ds)
    | d == (head ds) = d + go ds
    | otherwise = go ds
