{-# LANGUAGE OverloadedStrings #-}

module Hazel.CompletionSpec (spec)
       where

import Data.List (intersect)

import Prelude hiding (succ)
import Control.Monad.State.Lazy ( runState
                                , evalState
                                )
import Test.Hspec

import Hazel.Core ( Concept (..)
                  , Role (..))
import Hazel.Completion
import Hazel.TestCases

iS :: CState
iS = CState [] []

spec :: Spec
spec = do
  describe "Completion Graph Initialization" $
    it "initializes the completion graph" $ do
      let CGraph initS _ = iG
      initS name `shouldBe` [name]
  describe "Completion Rules" $ do
    describe "CR1 with Top" $ do
      let (CGraph n _, CState ns _) = (flip runState) iS $ cr1 cr1Gci iG top
      it "should find a new node" $
        null ns `shouldBe` False
      it "should find Person and Top" $
        n name `shouldBe` [top, name]
    describe "CR1 with Person" $ do
      let (CGraph n _, CState ns _) = (flip runState) iS $ cr1 cr1Gci iG name
      it "should not find a new node" $
        null ns `shouldBe` True
      it "should have Person and Top" $
        n name `shouldBe` [name]
      it "should have allNodes" $
        n top `shouldBe` top:allNodes
    describe "CR2" $ do
      let (CGraph n _, CState ns _) = (flip runState) iS $ cr2 cr2Gci iG name
      it "should find a new successor" $
        null ns `shouldBe` False
      it "should find Person, and Human" $
        n (Name "Human") `shouldBe` [name, Name "Human"]
      it "should find allNodes" $
        n top `shouldBe` top:allNodes
      it "should find Dummy and Top" $
        n (Name "Dummy") `shouldBe` [Name "Dummy"]
    describe "CR3" $ do
      let (CGraph _ n, CState _ rs) = (flip runState) iS $ cr3 cr3Gci iG name
      it "should find a new role pair" $
        null rs `shouldBe` False
      it "should find (Person, Person)" $
        n role `shouldBe` [(name, name)]
      it "should not find any pairs" $
        n (Role "marriedTo") `shouldBe` []
    describe "CR4" $ do
      let cgraph3 = (flip evalState) iS $ cr3 cr3Gci iG name
          (CGraph n _, CState ns _ ) = (flip runState) iS $ cr4 cr4Gci cgraph3 (name, name)
      it "should find a new successor" $
        null ns `shouldBe` False
      it "should find Person, and Father" $
        n (Name "Father") `shouldBe` [name, Name "Father"]
      it "should find allNodes" $
        n top `shouldBe` top:allNodes
    describe "Exercise 38" $ do
      let cg38 = complete normalized38
      it "Subsumees of A should be A" $
        getNodes cg38 a `intersect` [a, b, c, d] `shouldBe` [a]
      it "Subsumees of B should be A, B, C" $
        getNodes cg38 b `intersect` [a, b, c, d] `shouldBe` [a, b, c]
      it "Subsumees of C should be A, C" $
        getNodes cg38 b `intersect` [a, b, c, d] `shouldBe` [a, c]
      it "Subsumees of D should be A, C, D" $
        getNodes cg38 b `intersect` [a, b, c, d] `shouldBe` [a, c, d]
  where iG = initGraph allNodes
