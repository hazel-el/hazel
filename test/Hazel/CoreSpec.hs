module Hazel.CoreSpec (spec)
       where

import Test.Hspec

import Hazel.Core ()
import Hazel.TestCases

spec :: Spec
spec = describe "Show instances" $ do
  it "shows plain role names" $
    show role `shouldBe` "hasChild"
  it "shows plain concept names" $
    show top `shouldBe` "Thing"
  it "shows conjuctions" $
    show conjunction `shouldBe` "(Thing and Person)"
  it "shows existentials" $
    show existential `shouldBe` "(hasChild some Person)"
  it "shows GCIs" $
    show gci `shouldBe` "(hasChild some Person) SubClassOf Person"
  it "shows TBoxes" $
    show tbox `shouldBe` "[(hasChild some Person) SubClassOf Person,Thing SubClassOf (Thing and Person)]"
