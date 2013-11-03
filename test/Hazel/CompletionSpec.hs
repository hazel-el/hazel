{-# LANGUAGE OverloadedStrings #-}

module Hazel.CompletionSpec (spec)
       where

import Prelude hiding (succ)
import Test.Hspec

import Hazel.Core ( Concept (..)
                  , Role (..))
import Hazel.Completion
import Hazel.TestCases

spec :: Spec
spec = do
  describe "Completion Graph Initialization" $
    it "initializes the completion graph" $ do
      let CGraph initS _ = initGraph
      initS name `shouldBe` [name, top]
  describe "Completion Rules" $ do
    describe "CR1 with Top" $ do
      let (CGraph succ _, found) = cr1 cr1_gci initGraph top
      it "should find a new successor" $
        found `shouldBe` True
      it "should find Person and Top" $
        succ top `shouldBe` [name, top]
    describe "CR1 with Person" $ do
      let (CGraph succ _, found) = cr1 cr1_gci initGraph name
      it "should not find a new successor" $
        found `shouldBe` False
      it "should have Person and Top" $
        succ name `shouldBe` [name, top]
      it "should have Top" $
        succ top `shouldBe` [top]
    describe "CR2" $ do
      let (CGraph succ _, found) = cr2 cr2_gci initGraph name
      it "should find a new successor" $
        found `shouldBe` True
      it "sholud find Human, Person, and Top" $
        succ name `shouldBe` [Name "Human", name, top]
      it "should find Top" $
        succ top `shouldBe` [top]
      it "should find Dummy and Top" $
        succ (Name "Dummy") `shouldBe` [(Name "Dummy"), top]
    describe "CR3" $ do
      let (CGraph _ succ, found) = cr3 cr3_gci initGraph name
      it "should find a new role pair" $
        found `shouldBe` True
      it "should find (Person, Person)" $
        succ role `shouldBe` [(name, name)]
      it "should not find any pairs" $
        succ (Role "marriedTo") `shouldBe` []
    describe "CR4" $ do
      let cgraph3 = fst $ cr3 cr3_gci initGraph name
          (CGraph succ _, found) = cr4 cr4_gci cgraph3 name name
      it "should find a new successor" $
        found `shouldBe` True
      it "should find Father, Person, and Top" $
        succ name `shouldBe` [Name "Father", name, top]
      it "should find Top" $
        succ top `shouldBe` [top]
