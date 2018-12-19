{-# LANGUAGE OverloadedStrings #-}
module SeedSpec (spec) where

import Test.Hspec

import QB.Seed

spec :: Spec
spec = do
  describe "Unit test" $ do
    let seed = Seed { scheme = "sl4th3"
                    , axes = ["x","y"]
                    , bases = ["r","u","v","p"]
                    , elemType = "double"
                    , eomRank = 2
                    , withFilter = False
                    , params = Nothing
                    , initialCondition = Nothing
                    }
        -- x だけだと bool値 として解釈される
        yaml = "scheme: sl4th3\naxes: ['x','y']\nbases: [r, u, v, p]\nelem-type: double\neom-rank: 2\nwith-filter: false\n"
    it "check decoding yaml" $
      decodeSeed yaml `shouldBe` (Just seed)
