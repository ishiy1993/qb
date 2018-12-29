{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module QB.Exp where

import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as M
import System.IO

import QB.Pretty

type Ident = String

type Equations = [Equation]

data Equation = Equation
  { lhs :: Exp
  , rhs :: Exp
  } deriving (Show)

data Exp = Imm Double
         | Sym { name :: Ident
               , kind :: SymKind
               , differentiateBy :: Coords
               }
         | Neg Exp
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
  deriving (Show)

data SymKind = Constant
             | Variable
  deriving (Show)

type Coords = M.Map Coord Int

data Coord = I0 | I1 | I2 | I3
  deriving (Eq, Ord, Show,Enum)

build :: [Coord] -> Coords
build = foldr (\c acc -> M.insertWith (+) c 1 acc) M.empty

class Format a where
  formatWith :: [String] -> a -> B.Builder

instance Format Equations where
  formatWith as = sepWith "\n" . map (formatWith as)

instance Format Equation where
  formatWith as (Equation {lhs = l, rhs = r}) = formatWith as l <=> formatWith as r

instance Format Exp where
  formatWith _ (Imm x) = B.doubleDec x
  formatWith as (Sym n _ ds) = B.stringUtf8 n <> formatWith as ds
  formatWith as (Neg e) = "-" <> formatWith as e
  formatWith as (Add e1 e2) = formatOp as "+" e1 e2
  formatWith as (Sub e1 e2) = formatOp as "-" e1 e2
  formatWith as (Mul e1 e2) = formatOp as "*" e1 e2
  formatWith as (Div e1 e2) = formatOp as "/" e1 e2
  formatWith as (Pow e1 e2) = formatOp as "**" e1 e2

instance Format Coords where
  formatWith as ds | M.null ds = ""
                   | otherwise = "_" <> mconcat [mconcat $ replicate i (formatWith as d) | (d,i) <- M.toAscList ds]

instance Format Coord where
  formatWith as I0 = B.stringUtf8 $ as !! 0
  formatWith as I1 = B.stringUtf8 $ as !! 1
  formatWith as I2 = B.stringUtf8 $ as !! 2
  formatWith as I3 = B.stringUtf8 $ as !! 3

printWith :: Format a => [String] -> a -> IO ()
printWith as = B.hPutBuilder stdout . formatWith as

formatOp :: Format a => [String] -> B.Builder -> a -> a -> B.Builder
formatOp as op e1 e2 = "(" <> formatWith as e1 <> " " <> op <> " " <> formatWith as e2 <> ")"
