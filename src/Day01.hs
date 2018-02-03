-- Day01.hs
module Day01 where

import Data.Char (digitToInt)

-- |`captcha` returns the captcha for the given number.
-- Algorithm goes like this ...
-- * turn the int into a List of Chars
-- * append the first element of the list to the end of the list
-- * move a sliding window of two elements over the list
-- * add up all windows where the elements are the same
captcha :: Int -> Int
captcha i = go (prep i) where
  prep i = do
    let digits = map digitToInt (show i)
    digits ++ [head digits]
  go (i : []) = 0
  go (i : is)
    | i == (head is) = i + go is
    | otherwise = go is
