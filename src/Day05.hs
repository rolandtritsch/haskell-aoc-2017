{-|
Problem: <https://adventofcode.com/2017/day/5>

Solution:

General - Walk through the stack and update the stack with the offset()
function until the stack counter is/runs out of bounds (which indicates
that the "program" has exited.

Note: The given program will exit at the higher end of the stack, but
for completeness we are also checking for the lower bound.

Part1 - Trivial. Just increase the stack counter by 1.

Part2 - Increase the stack counter as described in the problem statement.
-}
module Day05 where

import Util (inputRaw)

-- | read the input
input :: [Int]
input = map read $ inputRaw "input/Day05input.txt"
