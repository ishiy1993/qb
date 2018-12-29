{-# LANGUAGE OverloadedStrings #-}
module QB.Pretty where

import qualified Data.ByteString.Builder as B

sepWith :: B.Builder -> [B.Builder] -> B.Builder
sepWith _ [] = mempty
sepWith s (x:xs) = x <> mconcat [s <> y | y <- xs]

paren :: [B.Builder] -> B.Builder
paren xs = "("  <> sepWith "," xs <> ")"

bckt :: [B.Builder] -> B.Builder
bckt xs = "["  <> sepWith "," xs <> "]"

(<=>) :: B.Builder -> B.Builder -> B.Builder
x <=> y = x <> " = " <> y

