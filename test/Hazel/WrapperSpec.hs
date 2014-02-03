{-# LANGUAGE OverloadedStrings #-}

module Hazel.WrapperSpec (spec)
       where

import Test.Hspec

import Hazel.Parser
import Hazel.TestCases
import Hazel.Wrapper

spec :: Spec
spec = describe "Testing parser and wrapper" $ it "GCIs should correspond to Exercise 38" $
       do Right o <- parseFile "test/data/exercise38.owl"
          extractGCIs o `shouldBe` exercise38
