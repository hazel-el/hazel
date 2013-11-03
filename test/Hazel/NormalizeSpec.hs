module Hazel.NormalizeSpec (spec)
       where

import Data.Monoid ((<>))
import Test.Hspec

import Hazel.Core
import Hazel.Normalize
import Hazel.TestCases

spec :: Spec
spec = do
  describe "GCI Normalization" $ do
    it "normalizes gci2" $
      normalizeGCI gci2 `shouldBe` gci2'
    it "normalizes gci2b" $
      normalizeGCI gci2b `shouldBe` gci2b'
    it "normalizes gci3" $
      normalizeGCI gci3 `shouldBe` gci3'
    it "normalizes gci3b" $
      normalizeGCI gci3b `shouldBe` gci3b'
    it "normalizes gci4" $
      normalizeGCI gci4 `shouldBe` gci4'
    it "normalizes gci4b" $
      normalizeGCI gci4b `shouldBe` gci4b'
    it "normalizes gci5" $
      normalizeGCI gci5 `shouldBe` gci5'
    it "normalizes gci5b" $
      normalizeGCI gci5b `shouldBe` gci5b'
    it "normalizes gci5c" $
      normalizeGCI gci5c `shouldBe` gci5c'
    it "normalizes gci5d" $
      normalizeGCI gci5d `shouldBe` gci5d'
    it "normalizes gci6" $
      normalizeGCI gci6 `shouldBe` gci6'
  describe "Signature computation" $ do
    it "computes a signature" $ do
      let (TBox _ cs rs) = normalizeGCI gci3b <> normalizeGCI gci5c
      (cs, rs) `shouldBe` signature3b5c
  describe "TBox normalization" $ do
    it "normalizes a TBox" $
      normalize [gci3b, gci5c] `shouldBe` normalizeGCI gci3b <> normalizeGCI gci5c
