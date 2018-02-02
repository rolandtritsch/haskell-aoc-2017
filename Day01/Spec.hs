-- Spec.hs
module Spec where

import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "captcha" $ do
    it "should return the correct result(s)" $ do
      Solution.captcha 1122 `shouldBe` 3
