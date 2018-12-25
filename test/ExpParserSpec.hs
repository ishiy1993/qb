module ExpParserSpec (spec) where

import Data.Either
import Test.Hspec

import QB.Exp.Parser

spec :: Spec
spec = do
  describe "Unit test" $ do
    it "check eqParser" $ do
      let eom = "q_t = q_xx + q_yy"
      parseWith ["t","x","y"] ["q"] eqParser "" eom `shouldSatisfy` isRight

    it "check eqsParser" $ do
      let eom = unlines [ "r_t = -u*r_x - v*r_y - r*(u_x + v_y)"
                        , "u_t = -u*u_x - v*u_y - p_x/r"
                        , "v_t = -u*v_x - v*v_y - p_y/r"
                        , "p_t = -u*p_x - v*p_y - gm*p*(u_x + v_y)"
                        ]
      let res = parseWith ["t","x","y"] ["r","u","v","p"] eqsParser "" eom
      res `shouldSatisfy` isRight
