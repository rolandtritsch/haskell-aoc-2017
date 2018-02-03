-- Day01Spec.hs
module Day01Spec where

import Test.Hspec

import Day01

run :: IO ()
run = hspec $ do
  describe "captcha" $ do
    it "returns the right result(s)" $ do
      Day01.captcha 1122 `shouldBe` 3
      Day01.captcha 1111 `shouldBe` 4
      Day01.captcha 1234 `shouldBe` 0
      Day01.captcha 91212129 `shouldBe` 9
