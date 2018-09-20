{-|
Problem: <https://adventofcode.com/2017/day/9>

Solution:

General - My first implementation was based on a recursive processing
of the input stream. Hard to read. Hard to understand. Hard to extend/
maintain. And hard to run (needs a special -Xss setting (more/large
stack).

This (refactored) implementation is using a state machine. The states
are OutOfGroup, InGroup, InGarbage, InCanceled. See also
<https://www.dropbox.com/s/pwogl9jhc8x7rqe/2018-02-213.01.24.jpg?dl=0 pic>.
While I am running the state machine I am collecting stats that will allow
me (at the end) to answer (the given) questions about the input stream.

Part1 - Trivial. Collect and show the right stats.

Part2 - Trivial. Collect and show the right stats.
-}
module Day09 where

import Util

-- | read the input
input :: String
input = head $ inputRaw "input/Day09input.txt"

type Level = Int
data Stats = Stats {
  score :: Int,
  numOfChars :: Int
  } deriving (Eq, Show)

-- | all of the states (according to the diagram)
data State
  = InGroup Level Stats
  | InGarbage Level Stats
  | InCanceled Level Stats
  deriving (Eq, Show)

-- | transition to the next state (according to the diagram)
transition :: State -> Char -> State
transition (InGroup level stats) '{' = InGroup (level + 1) stats
transition (InGroup level (Stats score chars)) '}' = InGroup (level - 1) (Stats (score + level) chars)
transition (InGroup level stats) '<' = InGarbage level stats
transition (InGroup level stats) _ = InGroup level stats
transition (InGarbage level stats) '>' = InGroup level stats
transition (InGarbage level stats) '!' = InCanceled level stats
transition (InGarbage level (Stats score chars)) _ = InGarbage level (Stats score (chars + 1))
transition (InCanceled level stats) _ = InGarbage level stats
