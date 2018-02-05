-- Day01Spec.hs
module Day01Spec where

import Test.Hspec

import Day01

run :: IO ()
run = hspec $ do
  describe "captcha" $ do
    it "returns the right result(s)" $ do
      Day01.captcha "1122" `shouldBe` 3
      Day01.captcha "1111" `shouldBe` 4
      Day01.captcha "1234" `shouldBe` 0
      Day01.captcha "91212129" `shouldBe` 9

  describe "captcha'" $ do
    it "returns the right result(s)" $ do
      Day01.captcha' "1212" `shouldBe` 6
      Day01.captcha' "1221" `shouldBe` 0
      Day01.captcha' "123425" `shouldBe` 4
      Day01.captcha' "123123" `shouldBe` 12
      Day01.captcha' "12131415" `shouldBe` 4
