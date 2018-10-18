{-|
Problem: <https://adventofcode.com/2017/day/18>

Solution:

General - First we need to implement all of the operations. We then
execute the progrom by executing all instructions (the input). Running
the program can be parameterized by `when is the program done?` and
`what do we do when we exit?`.

Note: To solve the puzzle(s) we need two program instances to talk
to each other (send/receive register values (frequencies)). For that
to happen I am using a blocking queue between the instances.

Note: The instruction on line 35 is >jgz 1 3<. Not sure, if this is
a typo. I changed it to >jgz l 3< and added >set l 1< as the first
instruction in the input file.

Part1 - Simple. Run the program. When we exit we need to return the
value of the recovered frequency (the value of the most recently
played sound; basically most recent value in the queue). We are done,
the first time a receive instruction is executed with a non-zero value.

Part2 - Also (kind of) simple (after some refactoring; using Future and
replacing LinkedBlockingDeque with Queue). Run until both sides are waiting
(i.e. are deadlocked) and (at that point in time) return/exit with the
writeCount.
-}
module Day18 where

import qualified Data.Map.Strict as M

import Util

type Register = Char
type Value = Int
type Registers = M.Map Register Value

type Instruction = State -> State
type Counter = Int

-- | the program state (pc, regs, done, exit)
type State = (Counter, Registers, Bool, Int)

-- | read the input
input :: [String]
input = inputRaw "input/Day18input.txt"

-- | a/the special register for a/the sound value
soundRegister :: Char
soundRegister = 'S'

-- | a/the special register for a/the sound recovery value
recoveryRegister :: Char
recoveryRegister = 'R'

-- | excute the snd (send) instruction
snd' :: Value -> State -> State
snd' frequency (pc, rs, _, _) =  (pc + 1, M.insert soundRegister frequency rs, False, 0)

-- | execute the set instruction
set' :: Register -> Value -> State -> State
set' r value (pc, rs, _, _) = (pc + 1, M.insert r value rs, False, 0)

-- | execute the add instruction
add' :: Register -> Value -> State -> State
add' r value (pc, rs, _, _) = (pc + 1, M.insert r ((M.findWithDefault 0 r rs) + value) rs, False, 0)

-- | execute the mul instruction
mul' :: Register -> Value -> State -> State
mul' r value (pc, rs, _, _) = (pc + 1, M.insert r ((M.findWithDefault 0 r rs) * value) rs, False, 0)

-- | execute the mod instruction
mod' :: Register -> Value -> State -> State
mod' r value (pc, rs, _, _) = (pc + 1, M.insert r (mod (M.findWithDefault 0 r rs) value) rs, False, 0)

-- | execute the rcv (receive) instruction
rcv' :: Register -> State -> State
rcv' r (pc, rs, _, _)
  | (M.findWithDefault 0 r rs) == 0 = (0, rs, True, rs M.! soundRegister)
  | otherwise = (pc + 1, rs, False, 0)

-- | execute the jgz (jump, if greater than zero) instruction
jgz' :: Register -> Int -> State -> State
jgz' r offset (pc, rs, _, _)
  | (M.findWithDefault 0 r rs) > 0 = (pc + offset, rs, False, 0)
  | otherwise = (pc + 1, rs, False, 0)

-- | build a/the program (a/the list of instructions) (from the input)
instructions :: [String] -> [Instruction]
instructions input' = map (instruction . words) input' where
  instruction ("snd":arguments) = snd' (read (arguments !! 0))
  instruction ("set":arguments) = set' ((arguments !! 0) !! 0) (read (arguments !! 1))
  instruction ("add":arguments) = add' ((arguments !! 0) !! 0) (read (arguments !! 1))
  instruction ("mul":arguments) = mul' ((arguments !! 0) !! 0) (read (arguments !! 1))
  instruction ("mod":arguments) = mod' ((arguments !! 0) !! 0) (read (arguments !! 1))
  instruction ("rcv":arguments) = rcv' ((arguments !! 0) !! 0)
  instruction ("jgz":arguments) = jgz' ((arguments !! 0) !! 0) (read (arguments !! 1))
  instruction _ = error "Unknown instruction"

 -- | run the instructions (until we are done)
run :: State -> [Instruction] -> Int
run (_, _, True, exit) _ = exit
run currentState@(pc, _, _, _) program = run nextState program where
  nextState = (program !! pc) currentState
