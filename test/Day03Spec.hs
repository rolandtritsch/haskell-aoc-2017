-- Day03Spec.hs
module Day03Spec where

import Test.Hspec

import Day03
import qualified Day03.Part1 as P1
import qualified Day03.Part2 as P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should read the (raw) input" $ do
      input `shouldBe` 368078

  describe "moves" $ do
    it "should create the right moves" $ do
      take 24 moves `shouldBe` [
        MoveRight,
        MoveUp,
        MoveLeft,MoveLeft,
        MoveDown,MoveDown,
        MoveRight,MoveRight,
        MoveRight,
        MoveUp,MoveUp,MoveUp,
        MoveLeft,MoveLeft,MoveLeft,MoveLeft,
        MoveDown,MoveDown,MoveDown,MoveDown,
        MoveRight,MoveRight,MoveRight,MoveRight
        ]

  describe "cells" $ do
    it "should create the right sequence of cells" $ do
      take 2 (cells moves) `shouldBe` [Cell 1 (0, 0), Cell 2 (0, 1)]

  describe "solve - part1" $ do
    it "should solve the testcase(s)" $ do
      P1.solve 1 `shouldBe` 0
      P1.solve 12 `shouldBe` 3
      P1.solve 23 `shouldBe` 2
      P1.solve 1024 `shouldBe` 31

    it "should solve the puzzle" $ do
      P1.solve input `shouldBe` 371

  describe "solve - part2" $ do
    it "should solve the puzzle" $ do
      P2.solve input `shouldBe` 369601
