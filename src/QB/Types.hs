{-# LANGUAGE OverloadedStrings #-}
module QB.Types where

import qualified Data.ByteString.Builder as B

type FmrType = String

type Code = B.Builder

data CodeStructure = CodeStructure
  { target :: String
  , axes :: [B.Builder]
  , gridStruct :: [B.Builder]
  , elemType :: B.Builder
  , defs :: [B.Builder]
  , initBody :: [B.Builder]
  , firstStepBody :: Maybe [B.Builder]
  , filterBody :: Maybe [B.Builder]
  , stepBody :: [B.Builder]
  }

format :: CodeStructure -> Code
format code = mconcat [ "dimension :: " <> (B.intDec . length $ axes code) <> B.charUtf8 '\n'
                      , "axes :: " <> (sepWith "," $ axes code) <> B.charUtf8 '\n'
                      , "" <> B.charUtf8 '\n'
                      , (sepWith "\n" $ defs code) <> B.charUtf8 '\n'
                      , "" <> B.charUtf8 '\n'
                      , defFun "init" "()" (paren gs) ([elemType code <> "[] :: " <> (sepWith "," [q <=> "0" | q <- gs])] <> initBody code) <> B.charUtf8 '\n'
                      , defFun' "first_step" (paren gs) (paren gs') (firstStepBody code) <> B.charUtf8 '\n'
                      , defFun' "filter" (paren gs) (paren gs') (filterBody code) <> B.charUtf8 '\n'
                      , defFun "step" (paren gs) (paren gs') (stepBody code) <> B.charUtf8 '\n'
                      ]
  where
    gs = gridStruct code
    gs' = map (<>"'") $ gridStruct code

sepWith :: B.Builder -> [B.Builder] -> B.Builder
sepWith _ [] = mempty
sepWith s (x:xs) = x <> mconcat [s <> y | y <- xs]

paren :: [B.Builder] -> B.Builder
paren xs = "("  <> sepWith "," xs <> ")"

bckt :: [B.Builder] -> B.Builder
bckt xs = "["  <> sepWith "," xs <> "]"

defFun :: B.Builder -> B.Builder -> B.Builder -> [B.Builder] -> B.Builder
defFun fn args res body = ("begin function " <> res <=> fn <> args) <> "\n"
                          <> sepWith "\n" body <> "\n" <>
                          "end function"

defFun' :: B.Builder -> B.Builder -> B.Builder -> Maybe [B.Builder] -> B.Builder
defFun' fn args res mbody = maybe "" (defFun fn args res) mbody

(<=>) :: B.Builder -> B.Builder -> B.Builder
x <=> y = x <> " = " <> y

(@=) :: B.Builder -> [B.Builder] -> B.Builder
q @= xs = q <=> paren xs

(=@) :: [B.Builder] -> B.Builder -> B.Builder
qs =@ x = paren qs <=> x

