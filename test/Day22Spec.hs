module Day22Spec where

import Test.Hspec

import Prelude hiding (Left, Right)

import Day22
import qualified Day22.Part1 as P1
import qualified Day22.Part2 as P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should read the (raw) input" $ do
      let (GridState grid mp _ _) = input
      head grid `shouldBe` [Clean,Clean,Clean,Infected,Infected,Infected,Clean,Infected,Clean,Infected,Clean,Infected,Infected,Clean,Clean,Clean,Infected,Infected,Clean,Infected,Clean,Clean,Infected,Infected,Clean]
      mp `shouldBe` (Position 12 12)

  describe "runSimulation" $ do
    it "should process the testcase(s) correctly" $ do
      let testGrid = [
            [Clean, Clean, Infected],
            [Infected, Clean, Clean],
            [Clean, Clean, Clean]
            ]
      let test = GridState testGrid (Position 1 1) Up 0
      let burst1Grid = [
            [Clean, Clean, Infected],
            [Infected, Infected, Clean],
            [Clean, Clean, Clean]
            ]
      let burst1 = GridState burst1Grid (Position 1 0) Left 1
      runSimulation 7 test `shouldBe` burst1

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      P1.solve input `shouldBe` 5305

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      P2.solve input `shouldBe` 2511424
