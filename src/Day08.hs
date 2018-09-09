{-|
Problem: <https://adventofcode.com/2017/day/8>

Solution:

General - Very simple idea. We have a set of (immutable) instructions (no jumps)
and a set of registers. We just need to go through all instructions and update
the set of registers on the way (with every instruction). While we do this, we
collect all register states. The final register state is the head of the collected
register states.

Part1 - Simple. The max of the register values in the final/last register state/set.

Part2 - Simple. The max of all register values in all collected register states/sets.
-}
module Day08 where

import Data.List.Split (splitOneOf)
import Data.Maybe (fromJust)

import qualified Data.Map as M

import Util (inputRaw)

-- | the registers
type Registers = M.Map String Int

-- | an instruction
data Instruction = Instruction {
  register :: String,
  operation :: String,
  operand :: Int,
  conditionRegister :: String,
  condition :: String,
  conditionOperand :: Int
  } deriving (Eq, Show)

-- | read the input
input :: [Instruction]
input = map parser $ inputRaw "input/Day08input.txt" where
  parser line = Instruction reg oper oped creg cond coped where
    -- g dec 231 if bfx > -10
    tokens = filter ((/=) "if") $ splitOneOf "[] " line
    reg = tokens !! 0
    oper = tokens !! 1
    oped = read $ tokens !! 2
    creg = tokens !! 3
    cond = tokens !! 4
    coped = read $ tokens !! 5

-- | test, if the instruction is to be executed
doIt :: Registers -> Instruction -> Bool
doIt rs (Instruction _ _ _ creg "==" coped) = (M.findWithDefault 0 creg rs) == coped
doIt rs (Instruction _ _ _ creg "!=" coped) = (M.findWithDefault 0 creg rs) /= coped
doIt rs (Instruction _ _ _ creg "<" coped) = (M.findWithDefault 0 creg rs) < coped
doIt rs (Instruction _ _ _ creg ">" coped) = (M.findWithDefault 0 creg rs) > coped
doIt rs (Instruction _ _ _ creg "<=" coped) = (M.findWithDefault 0 creg rs) <= coped
doIt rs (Instruction _ _ _ creg ">=" coped) = (M.findWithDefault 0 creg rs) >= coped

-- | exec a given instruction on the registers (and return the updated registers)
exec :: Registers -> Instruction -> Registers
exec rs i@(Instruction reg "inc" oped _ _ _)
  | doIt rs i = M.insert reg ((M.findWithDefault 0 reg rs) + oped) rs
  | otherwise = rs
exec rs i@(Instruction reg "dec" oped _ _ _)
  | doIt rs i = M.insert reg ((M.findWithDefault 0 reg rs) - oped) rs
  | otherwise = rs
