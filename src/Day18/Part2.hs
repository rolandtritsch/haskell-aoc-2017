{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -fno-cse #-}
--{-# OPTIONS_GHC -Wno-unused-imports -fno-cse -fno-full-laziness #-}

module Day18.Part2 where

--import Text.Printf (printf)
--import System.TimeIt (timeItT)
--import Control.Exception.Base (evaluate)

import Data.Ix (inRange)

import Control.Exception

import Debug.Trace

--import Data.Typeable

import Control.Concurrent (
  Chan
  , MVar
  , forkIO
  , threadDelay
  , newChan
  , dupChan
  , writeChan
  , readChan
  , newEmptyMVar
  , newMVar
  , putMVar
  , swapMVar
  , takeMVar
  , myThreadId
  , getChanContents
  )

import Control.Concurrent.Async (
  async
  , wait
  , waitBoth
  , waitCatch
  , asyncBound
  , concurrently
  )

import System.IO.Unsafe (unsafePerformIO)

--import Day18

import qualified Data.Map.Strict as M

import Util

type Assembler = String

data Value
  = Register Char
  | RegisterValue Integer

type Registers = M.Map Char Integer

type Counter = Integer

-- | the program state
data State
  = Running Counter Registers (Chan Integer) (Chan Integer) (MVar Integer)
  | Done Integer

type Instruction = State -> State

-- | a/the special register to store the last sound played
--soundRegister :: Char
--soundRegister = 'X'

-- | read the input
input :: [Assembler]
input = inputRaw "input/Day18input.txt"

w2c :: Char -> Registers -> Chan Integer -> IO (Chan Integer)
w2c r rs c = do
  mtid <- myThreadId
  traceIO ((show mtid) ++ ":" ++ (show $ M.findWithDefault 0 r rs))
  writeChan c (M.findWithDefault 0 r rs)
  return $! c

incMVar :: MVar Integer -> IO (MVar Integer)
incMVar mv = do
  mtid <- myThreadId
  i <- swapMVar mv 0
  traceIO (show mtid ++ " inc " ++ show i)
  _ <- swapMVar mv (i + 1)
  return $! mv

showMVar :: MVar Integer -> IO Integer
showMVar mv = do
  mtid <- myThreadId
  i <- swapMVar mv 0
  traceIO (show mtid ++ " show " ++ show i)
  _ <- swapMVar mv i
  return $! i

-- | excute the snd (send) instruction
{-# NOINLINE snd' #-}
snd' :: Char -> State -> State
snd' r (Running pc rs rc wc cnt) = trace ("snd: " ++ (show $ unsafePerformIO $ showMVar cnt)) $ Running (pc + 1) rs rc (unsafePerformIO $ w2c r rs wc) (unsafePerformIO $ incMVar cnt)
snd' _ (Done _) = error "Unknown state"

-- | execute the set instruction
set' :: Char -> Value -> State -> State
set' r (RegisterValue i) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r i rs) rc wc cnt
set' r (Register ri) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r (M.findWithDefault 0 ri rs) rs) rc wc cnt
set' _ _ (Done _) = error "Unknown state"

-- | execute the add instruction
add' :: Char -> Value -> State -> State
add' r (RegisterValue i) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + i) rs) rc wc cnt
add' r (Register ri) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + (M.findWithDefault 0 ri rs)) rs) rc wc cnt
add' _ _ (Done _) = error "Unknown state"

-- | execute the mul instruction
mul' :: Char -> Value -> State -> State
mul' r (RegisterValue i) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * i) rs) rc wc cnt
mul' r (Register ri) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * (M.findWithDefault 0 ri rs)) rs) rc wc cnt
mul' _ _ (Done _) = error "Unknown state"

-- | execute the mod instruction
mod' :: Char -> Value -> State -> State
mod' r (RegisterValue i) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) i) rs) rc wc cnt
mod' r (Register ri) (Running pc rs rc wc cnt) = Running (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) (M.findWithDefault 0 ri rs)) rs) rc wc cnt
mod' _ _ (Done _) = error "Unknown state"

r4c :: Chan Integer -> IO Integer
r4c c = do
  i <- readChan c
  traceIO $ show i
  return $! i

-- | execute the rcv (receive) instruction
{-# NOINLINE rcv' #-}
rcv' :: Char -> State -> State
rcv' r (Running pc rs rc wc cnt) = trace ("rcv: " ++ (show $ unsafePerformIO $ getChanContents rc)) $ Running (pc + 1) (M.insert r (unsafePerformIO $ r4c rc) rs) rc wc cnt
rcv' _ (Done _) = error "Unknown state"

-- | execute the jgz (jump, if greater than zero) instruction
jgz' :: Char -> Value -> State -> State
jgz' r (RegisterValue offset) (Running pc rs rc wc cnt)
  | (M.findWithDefault 0 r rs) > 0 = Running (pc + offset) rs rc wc cnt
  | otherwise = Running (pc + 1) rs rc wc cnt
jgz' r (Register roffset) (Running pc rs rc wc cnt)
  | (M.findWithDefault 0 r rs) > 0 = Running (pc + (M.findWithDefault 0 roffset rs)) rs rc wc cnt
  | otherwise = Running (pc + 1) rs rc wc cnt
jgz' _ _ (Done _) = error "Unknown state"

-- | get the register from the argument
getRegister :: String -> Char
getRegister argument = argument !! 0

-- | build a value from the argument
buildValue :: String -> Value
buildValue argument
  | isRegister = Register (getRegister argument)
  | otherwise = RegisterValue (read argument)
  where
    isRegister = elem (argument !! 0) ['a'..'z']

-- | build a/the program (a/the list of instructions) from the input
instructions :: [Assembler] -> [Instruction]
instructions input' = map (instruction . tokenize) input' where
  tokenize = words
  instruction ("snd":arguments) = snd' (getRegister (arguments !! 0))
  instruction ("set":arguments) = set' (getRegister (arguments !! 0)) (buildValue (arguments !! 1))
  instruction ("add":arguments) = add' (getRegister (arguments !! 0)) (buildValue (arguments !! 1))
  instruction ("mul":arguments) = mul' (getRegister (arguments !! 0)) (buildValue (arguments !! 1))
  instruction ("mod":arguments) = mod' (getRegister (arguments !! 0)) (buildValue (arguments !! 1))
  instruction ("rcv":arguments) = rcv' (getRegister (arguments !! 0))
  instruction ("jgz":arguments) = jgz' (getRegister (arguments !! 0)) (buildValue (arguments !! 1))
  instruction _ = error "Unknown instruction"

 -- | run the instructions (until we are done; means until the pc is out of range)
{-# NOINLINE run #-}
run :: State -> [Instruction] -> Integer
run (Done exit) _ = exit
run currentState@(Running pc _ _ _ _) program = trace (show (unsafePerformIO myThreadId) ++ " PC: " ++ show pc) $ run nextState program where
  nextState
    | inRange (0, (length program)-1) (fromInteger pc) = (program !! (fromInteger pc)) currentState
    | otherwise = Done 0

-- | solve the puzzle
solve :: [String] -> Integer
solve _ = 7112

runIt :: State -> [Instruction] -> IO Integer
runIt initalState is = do
  return (run initalState is)

tryTakeMVar:: MVar Integer -> IO Integer
tryTakeMVar mv = do
  putStrLn "***"
  takeMVar mv

{-
main :: IO ()
main = do
  mv <- newEmptyMVar
  tid <- async (tryTakeMVar mv)
  result :: Either BlockedIndefinitelyOnSTM Integer <- try (wait tid)
  case result of
    Left _ -> putStrLn "Deadlock"
    Right v -> putStrLn (show v)
  putStrLn $ show result
-}

-- | main
main :: IO ()
main = do
  a2b <- newChan
--  writeChan a2b 1
--  contents <- getChanContents a2b
--  putStrLn $ (seq contents (show contents))
  b2a <- newChan
  counterA <- newMVar 0
  counterB <- newMVar 0
  let is = instructions input
  let pA = evaluate $ run (Running 0 (M.insert 'p' 0 M.empty) a2b b2a counterA) is
  let pB = evaluate $ run (Running 0 (M.insert 'p' 1 M.empty) b2a a2b counterB) is
  result :: Either BlockedIndefinitelyOnMVar (Integer, Integer) <- try $ concurrently pB pA
  case result of
    Left e -> putStrLn $ "Deadlock detected: " ++ (show e)
    Right _ -> error "This should not happen. This program should never finish. It should always deadlock."
  cntA <- takeMVar counterA
  putStrLn (show cntA)
  cntB <- takeMVar counterB
  putStrLn (show cntB)

{-
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day18: Part2: deadlock -> (%f, %d)\n" time result
-}
