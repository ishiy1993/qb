{-# LANGUAGE GADTs #-}
module QB.Exp where

import qualified Data.Map.Strict as M

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
