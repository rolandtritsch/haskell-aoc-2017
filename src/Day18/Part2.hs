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

import Control.Concurrent

import Control.Concurrent.Async

import Control.Concurrent.STM

import System.IO.Unsafe

--import Day18

import qualified Data.Map.Strict as M

import Util

type Assembler = String

data Value
  = Register Char
  | RegisterValue Integer

type Registers = M.Map Char Integer

type Counter = Integer

type TId = Integer

-- | the program state
data State
  = Running TId Counter Registers (TChan Integer) (TChan Integer) (TVar Integer)
  | Done Integer

type Instruction = State -> IO State

-- | a/the special register to store the last sound played
--soundRegister :: Char
--soundRegister = 'X'

-- | read the input
input :: [Assembler]
input = inputRaw "input/Day18input.txt"

-- | excute the snd (send) instruction
snd' :: Char -> State -> IO State
snd' r (Running tid pc rs rc wc cnt) = atomically $ do
  let v = M.findWithDefault 0 r rs
  writeTChan wc v
  i <- readTVar cnt
  trace ("snd:" ++ show tid ++ ":" ++ show v ++ ":" ++ show i) $ modifyTVar cnt (+1)
  return $ Running tid (pc + 1) rs rc wc cnt
snd' _ (Done _) = error "Unknown state"

-- | execute the set instruction
set' :: Char -> Value -> State -> IO State
set' r (RegisterValue i) (Running tid pc rs rc wc cnt) = return $ trace ("set:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r i rs) rc wc cnt
set' r (Register ri) (Running tid pc rs rc wc cnt) = return $ trace ("set:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (M.findWithDefault 0 ri rs) rs) rc wc cnt
set' _ _ (Done _) = error "Unknown state"

-- | execute the add instruction
add' :: Char -> Value -> State -> IO State
add' r (RegisterValue i) (Running tid pc rs rc wc cnt) = return $ trace ("add:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + i) rs) rc wc cnt
add' r (Register ri) (Running tid pc rs rc wc cnt) = return $ trace ("add:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + (M.findWithDefault 0 ri rs)) rs) rc wc cnt
add' _ _ (Done _) = error "Unknown state"

-- | execute the mul instruction
mul' :: Char -> Value -> State -> IO State
mul' r (RegisterValue i) (Running tid pc rs rc wc cnt) = return $ trace ("mul:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * i) rs) rc wc cnt
mul' r (Register ri) (Running tid pc rs rc wc cnt) = return $ trace ("mul:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * (M.findWithDefault 0 ri rs)) rs) rc wc cnt
mul' _ _ (Done _) = error "Unknown state"

-- | execute the mod instruction
mod' :: Char -> Value -> State -> IO State
mod' r (RegisterValue i) (Running tid pc rs rc wc cnt) = return $ trace ("mod:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) i) rs) rc wc cnt
mod' r (Register ri) (Running tid pc rs rc wc cnt) = return $ trace ("mod:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) (M.findWithDefault 0 ri rs)) rs) rc wc cnt
mod' _ _ (Done _) = error "Unknown state"

-- | execute the rcv (receive) instruction
rcv' :: Char -> State -> IO State
rcv' r (Running tid pc rs rc wc cnt) = atomically $ do
  v <- readTChan rc
  trace ("rcv:" ++ show tid ++ ":" ++ show v) $ return ()
  return $ Running tid (pc + 1) (M.insert r v rs) rc wc cnt
rcv' _ (Done _) = error "Unknown state"

-- | execute the jgz (jump, if greater than zero) instruction
jgz' :: Char -> Value -> State -> IO State
jgz' r (RegisterValue offset) (Running tid pc rs rc wc cnt)
  | (M.findWithDefault 0 r rs) > 0 = return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + offset) rs rc wc cnt
  | otherwise = return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) rs rc wc cnt
jgz' r (Register roffset) (Running tid pc rs rc wc cnt)
  | (M.findWithDefault 0 r rs) > 0 = return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + (M.findWithDefault 0 roffset rs)) rs rc wc cnt
  | otherwise = return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) rs rc wc cnt
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
run :: IO State -> [Instruction] -> IO State
run currentState program = do
  cs <- currentState
  case cs of
    (Done _) -> currentState
    (Running tid pc _ _ _ _) -> trace ("pc:" ++ show tid ++ ":" ++ show pc) $ run (currentState >>= nextState) program
      where
        nextState cs'@(Running tid' pc' _ _ _ _)
          | inRange (0, (length program)-1) (fromInteger pc') = trace ("ns:" ++ show tid' ++ ":" ++ show pc') $ (program !! (fromInteger pc')) cs'
          | otherwise = return $ Done 0
        nextState (Done _) = error "This should never happen (because I pattern match on Done above)."

{-
  nextState where
  nextState
    | inRange (0, (length program)-1) (fromInteger pc) = (program !! (fromInteger pc)) currentState
    | otherwise = return $ Done 0
-}
-- | solve the puzzle
solve :: [String] -> Integer
solve _ = 7112

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
  a2b <- newTChanIO
--  writeChan a2b 1
--  contents <- getChanContents a2b
--  putStrLn $ (seq contents (show contents))
  b2a <- newTChanIO
  counterA <- newTVarIO 0
  counterB <- newTVarIO 0
  let is = instructions input
  let pA = run (return $ Running 0 0 (M.insert 'p' 0 M.empty) a2b b2a counterA) is
  let pB = run (return $ Running 1 0 (M.insert 'p' 1 M.empty) b2a a2b counterB) is
  result :: Either BlockedIndefinitelyOnSTM (State, State) <- try $ concurrently pB pA
  case result of
    Left e -> putStrLn $ "Deadlock detected: " ++ (show e)
    Right _ -> error "This should not happen. This program should never finish. It should always deadlock."
  cntA <- readTVarIO counterA
  putStrLn (show cntA)
  cntB <- readTVarIO counterB
  putStrLn (show cntB)

{-
main = do
  (time, result) <- timeItT $ evaluate (solve input)
  printf "Day18: Part2: deadlock -> (%f, %d)\n" time result
-}
