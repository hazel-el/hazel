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
      let CGraph initS _ = iG
      initS name `shouldBe` [name]
  describe "Completion Rules" $ do
    describe "CR1 with Top" $ do
      let (CGraph n _, found) = (cr1 cr1_gci iG top, True)
      it "should find a new node" $
        found `shouldBe` True
      it "should find Person and Top" $
        n name `shouldBe` [top, name]
    describe "CR1 with Person" $ do
      let (CGraph n _, found) = (cr1 cr1_gci iG name, False)
      it "should not find a new node" $
        found `shouldBe` False
      it "should have Person and Top" $
        n name `shouldBe` [name]
      it "should have allNodes" $
        n top `shouldBe` top:allNodes
    describe "CR2" $ do
      let (CGraph n _, found) = (cr2 cr2_gci iG name, True)
      it "should find a new successor" $
        found `shouldBe` True
      it "should find Person, and Human" $
        n (Name "Human") `shouldBe` [name, Name "Human"]
      it "should find allNodes" $
        n top `shouldBe` top:allNodes
      it "should find Dummy and Top" $
        n (Name "Dummy") `shouldBe` [Name "Dummy"]
    describe "CR3" $ do
      let (CGraph _ n, found) = (cr3 cr3_gci iG name, True)
      it "should find a new role pair" $
        found `shouldBe` True
      it "should find (Person, Person)" $
        n role `shouldBe` [(name, name)]
      it "should not find any pairs" $
        n (Role "marriedTo") `shouldBe` []
    describe "CR4" $ do
      let cgraph3 = cr3 cr3_gci iG name
          (CGraph n _, found) = (cr4 cr4_gci cgraph3 (name, name), True)
      it "should find a new successor" $
        found `shouldBe` True
      it "should find Person, and Father" $
        n (Name "Father") `shouldBe` [name, Name "Father"]
      it "should find allNodes" $
        n top `shouldBe` top:allNodes
  where iG = initGraph allNodes
