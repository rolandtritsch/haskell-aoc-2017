-- Day08Spec.hs
module Day08Spec where

import Test.Hspec

import Day08
import qualified Day08.Part1 as P1
import qualified Day08.Part2 as P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should read the (raw) input" $ do
      head input `shouldBe` (Instruction "g" "dec" 231 "bfx" ">" (negate 10))

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      P1.solve input `shouldBe` 4163

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      P2.solve input `shouldBe` 5347