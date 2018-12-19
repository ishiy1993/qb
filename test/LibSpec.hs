module LibSpec (spec) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Unit test" $ do
    it "check program" $
      program `shouldBe` "good"
      
