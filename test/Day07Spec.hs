-- Day07Spec.hs
module Day07Spec where

import Test.Hspec

import Data.Maybe (fromJust)
import qualified Data.Map as M

import Data.Tree
import qualified Data.Tree as T

import Day07
import qualified Day07.Part1 as P1
import qualified Day07.Part2 as P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should read the (raw) input" $ do
      (fromJust $ M.lookup "occxa" input) `shouldBe` (60, [])
      (fromJust $ M.lookup "kozpul" input) `shouldBe` (59, ["shavjjt","anujsv","tnzvo"])

{--
  describe "build" $ do
    it "should build the tree" $ do
      build input (findRoot input) `shouldBe` (Node 0 [])

  describe "calcWeight" $ do
    it "should calculate the right weight" $ do
      calcWeight (build input Root (findRoot input)) `shouldBe` 387014

  describe "isBalanced" $ do
    it "should see that the tree is not balanced" $ do
      isBalanced (build input Root (findRoot input)) `shouldBe` False

  describe "findBadNode" $ do
    it "should find the (one/first) bad node" $ do
      name (findBadNode (build input Root (findRoot input))) `shouldBe` "zuahdoy"

  describe "correctWeight" $ do
    it "should return the correct weight for a/the bad node" $ do
      correctWeight (findBadNode (build input Root (findRoot input))) `shouldBe` 596
--}

  describe "solve - Part1" $ do
    it "should solve the puzzle" $ do
      P1.solve input `shouldBe` "uownj"

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      P2.solve input `shouldBe` 596
