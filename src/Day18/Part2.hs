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
import System.IO

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

type Instruction = IO State -> IO State

data S = S String Integer

-- | a/the special register to store the last sound played
--soundRegister :: Char
--soundRegister = 'X'

-- | read the input
input :: [Assembler]
input = inputRaw "input/Day18input.txt"

-- | excute the snd (send) instruction
snd' :: Char -> IO State -> IO State
snd' r s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> atomically $ do
      let v = M.findWithDefault 0 r rs
      writeTChan wc v
      i <- readTVar cnt
      trace ("snd:" ++ show tid ++ ":" ++ show v ++ ":" ++ show i) $ modifyTVar cnt (+1)
      return $ Running tid (pc + 1) rs rc wc cnt

-- | execute the set instruction
set' :: Char -> Value -> IO State -> IO State
set' r (RegisterValue i) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("set:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r i rs) rc wc cnt
set' r (Register ri) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("set:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (M.findWithDefault 0 ri rs) rs) rc wc cnt

-- | execute the add instruction
add' :: Char -> Value -> IO State -> IO State
add' r (RegisterValue i) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("add:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + i) rs) rc wc cnt
add' r (Register ri) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("add:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) + (M.findWithDefault 0 ri rs)) rs) rc wc cnt

-- | execute the mul instruction
mul' :: Char -> Value -> IO State -> IO State
mul' r (RegisterValue i) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("mul:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * i) rs) rc wc cnt
mul' r (Register ri) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("mul:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r ((M.findWithDefault 0 r rs) * (M.findWithDefault 0 ri rs)) rs) rc wc cnt

-- | execute the mod instruction
mod' :: Char -> Value -> IO State -> IO State
mod' r (RegisterValue i) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("mod:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) i) rs) rc wc cnt
mod' r (Register ri) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> return $ trace ("mod:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) (M.insert r (mod (M.findWithDefault 0 r rs) (M.findWithDefault 0 ri rs)) rs) rc wc cnt

-- | execute the rcv (receive) instruction
rcv' :: Char -> IO State -> IO State
rcv' r s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt -> atomically $ do
      v <- readTChan rc
      trace ("rcv:" ++ show tid ++ ":" ++ show v) $ return ()
      return $ Running tid (pc + 1) (M.insert r v rs) rc wc cnt

-- | execute the jgz (jump, if greater than zero) instruction
jgz' :: Char -> Value -> IO State -> IO State
jgz' r (RegisterValue offset) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt | (M.findWithDefault 0 r rs) > 0 -> return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + offset) rs rc wc cnt
    Running tid pc rs rc wc cnt -> return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) rs rc wc cnt
jgz' r (Register roffset) s = do
  s' <- s
  case s' of
    Done _ -> error "Unknown state"
    Running tid pc rs rc wc cnt | (M.findWithDefault 0 r rs) > 0 -> return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + (M.findWithDefault 0 roffset rs)) rs rc wc cnt
    Running tid pc rs rc wc cnt -> return $ trace ("jgz:" ++ show tid ++ ":" ++ show pc) $ Running tid (pc + 1) rs rc wc cnt

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
    (Running tid pc _ _ _ _) -> trace ("pc:" ++ show tid ++ ":" ++ show pc) $ run nextState program where
      nextState
        | inRange (0, (length program)-1) (fromInteger pc) = trace ("ns:" ++ show tid ++ ":" ++ show pc) $ (program !! (fromInteger pc)) currentState
        | otherwise = return $ Done 0

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

atom :: STM S -> STM S
atom s = do
  s' <- s
  case s' of
    S str i -> return $ S str (i + 1)

run' :: IO S -> IO S
run' s = do
  s' <- s
  case s' of
    S str i | i < 10 -> trace (show str ++ ":" ++ show i) $ run' (atomically (atom (return s')))
    S _ _ -> return $ S "Done" 0

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
  S str i <- run' (return $ S "test" 0)
  trace ((show str) ++ ":" ++ (show i)) $ return ()
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
