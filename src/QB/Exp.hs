{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module QB.Exp where

import qualified Data.ByteString.Builder as B
import Data.Function (fix)
import qualified Data.Map.Strict as M
import System.IO

import QB.Pretty

type Ident = String

type Equations = [Equation]

data Equation = Equation
  { lhs :: !Exp
  , rhs :: !Exp
  } deriving (Show)

data Exp = Imm !Double
         | Sym { name :: !Ident
               , kind :: !SymKind
               , differentiateBy :: !Coords
               }
         | Neg Exp
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
  deriving (Show,Eq)

data SymKind = Constant
             | Variable
  deriving (Show,Eq)

type Coords = M.Map Coord Int

data Coord = I0 | I1 | I2 | I3
  deriving (Eq, Ord, Show,Enum)

build :: [Coord] -> Coords
build = foldr (\c acc -> M.insertWith (+) c 1 acc) M.empty

isConstant :: Exp -> Bool
isConstant (Sym _ Variable _) = False
isConstant (Neg e) = isConstant e
isConstant (Add e1 e2) = isConstant e1 && isConstant e2
isConstant (Sub e1 e2) = isConstant e1 && isConstant e2
isConstant (Mul e1 e2) = isConstant e1 && isConstant e2
isConstant (Div e1 e2) = isConstant e1 && isConstant e2
isConstant (Pow e1 e2) = isConstant e1 && isConstant e2
isConstant _ = True

isImm :: Exp -> Bool
isImm (Imm _) = True
isImm (Neg e) = isImm e
isImm (Add e1 e2) = isImm e1 && isImm e2
isImm (Sub e1 e2) = isImm e1 && isImm e2
isImm (Mul e1 e2) = isImm e1 && isImm e2
isImm (Div e1 e2) = isImm e1 && isImm e2
isImm (Pow e1 e2) = isImm e1 && isImm e2
isImm _ = False

class Format a where
  formatWith :: [String] -> a -> B.Builder

instance Format Equations where
  formatWith as = sepWith "\n" . map (formatWith as)

instance Format Equation where
  formatWith as (Equation {lhs = l, rhs = r}) = formatWith as l <=> formatWith as r

instance Format Exp where
  formatWith _ (Imm x) = B.doubleDec x
  formatWith as (Sym n _ ds) = B.stringUtf8 n <> formatWith as ds
  formatWith as (Neg e) = "-(" <> formatWith as e <> ")"
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

class Diff a where
  diff :: Coord -> a -> a

instance Diff Equations where
  diff i es = [e | Just e <- map (diff' i) es]
    where diff' x e | isConstant (lhs e) = Nothing
                    | otherwise = Just $ diff x e

instance Diff Equation where
  diff i (Equation {lhs,rhs}) = Equation {lhs = diff i lhs, rhs = eval (diff i rhs)}

instance Diff Exp where
  diff _ (Imm _) = Imm 0.0
  diff _ (Sym _ Constant _) = Imm 0.0
  diff i s@(Sym _ Variable ds) = s { differentiateBy = M.insertWith (+) i 1 ds }
  diff i (Neg e) = Neg (diff i e)
  diff i (Add e1 e2) = Add (diff i e1) (diff i e2)
  diff i (Sub e1 e2) = Sub (diff i e1) (diff i e2)
  diff i (Mul e1 e2) = Add (Mul (diff i e1) e2) (Mul e1 (diff i e2))
  diff i (Div e1 e2) = Sub (Div (diff i e1) e2)
                           (Div (Mul e1 (diff i e2)) (Pow e2 (Imm 2)))
  diff i (Pow e1 e2) | isConstant e2 = Mul e2 (Mul (diff i e1) (Pow e1 (Sub e2 (Imm 1))))
                     | otherwise = error "Invalid exp"

eval :: Exp -> Exp
eval = id
-- fixExp :: (Exp -> Exp) -> Exp -> Exp
-- fixExp updater = fix (\f e -> let e' = updater e in if e' == e then e else f e')

-- eval :: Exp -> Exp
-- eval = fixExp eval'
--   where
--     eval' e = if isImm e then Imm (evalImm e) else evalExp e

--     evalImm (Imm x) = x
--     evalImm (Neg e) = negate (evalImm e)
--     evalImm (Add e1 e2) = evalImm e1 + evalImm e2
--     evalImm (Sub e1 e2) = evalImm e1 - evalImm e2
--     evalImm (Mul e1 e2) = evalImm e1 * evalImm e2
--     evalImm (Div e1 e2) = evalImm e1 / evalImm e2
--     evalImm (Pow e1 e2) = evalImm e1 ** evalImm e2

--     evalExp e@(Imm x) = if x < 0 then Neg (Imm $ negate x) else e
--     evalExp (Neg (Neg e)) = e
--     evalExp (Add (Imm 0) e2) = e2
--     evalExp (Add e1 (Imm 0)) = e1
--     evalExp (Add (Neg e1) (Neg e2)) = Neg (Add e1 e2)
--     evalExp (Add e1 (Neg e2)) = Sub e1 e2
--     evalExp (Sub (Imm 0) e2) = Neg e2
--     evalExp (Sub e1 (Imm 0)) = e1
--     evalExp (Sub (Neg e1) (Neg e2)) = Neg (Sub e1 e2)
--     evalExp (Sub e1 (Neg e2)) = Add e1 e2
--     evalExp (Mul (Imm 0) e2) = Imm 0
--     evalExp (Mul e1 (Imm 0)) = Imm 0
--     evalExp (Mul (Imm 1) e2) = e2
--     evalExp (Mul e1 (Imm 1)) = e1
--     evalExp (Mul (Neg e1) (Neg e2)) = Mul e1 e2
--     evalExp (Mul (Neg e1) e2) = Neg (Mul e1 e2)
--     evalExp (Mul e1 (Neg e2)) = Neg (Mul e1 e2)
--     evalExp (Div (Imm 0) e2) = Imm 0
--     evalExp (Div e1 (Imm 1)) = e1
--     evalExp (Div (Neg e1) (Neg e2)) = Div e1 e2
--     evalExp (Div (Neg e1) e2) = Neg (Div e1 e2)
--     evalExp (Div e1 (Neg e2)) = Neg (Div e1 e2)
