module QB.Types where

import Data.List (intercalate)

type FmrType = String

type Code = String

data CodeStructure = CodeStructure
  { target :: String
  , axes :: [String]
  , gridStruct :: [String]
  , elemType :: FmrType
  , defs :: [String]
  , initBody :: [String]
  , firstStepBody :: Maybe [String]
  , filterBody :: Maybe [String]
  , stepBody :: [String]
  }

format :: CodeStructure -> Code
format code = unlines [ "dimension :: " ++ (show . length $ axes code)
                    , "axes :: " ++ (intercalate "," $ axes code)
                    , ""
                    , unlines $ defs code
                    , ""
                    , defFun "init" "()" (paren gs) ([elemType code ++ "[] :: " ++ (intercalate "," [q <=> "0" | q <- gs])] ++ initBody code)
                    , defFun' "first_step" (paren gs) (paren gs') (firstStepBody code)
                    , defFun' "filter" (paren gs) (paren gs') (filterBody code)
                    , defFun "step" (paren gs) (paren gs') (stepBody code)
                    ]
  where
    gs = gridStruct code
    gs' = map (++"'") $ gridStruct code

paren :: [String] -> String
paren xs = "("  ++ intercalate "," xs ++ ")"

bckt :: [String] -> String
bckt xs = "["  ++ intercalate "," xs ++ "]"

defFun :: String -> String -> String -> [String] -> String
defFun fn args res body =
  unlines $ [ "begin function " ++ res <=> fn ++ args ]
            ++ body ++
            [ "end function" ]

defFun' :: String -> String -> String -> Maybe [String] -> String
defFun' fn args res mbody = maybe "" (defFun fn args res) mbody


(<=>) :: String -> String -> String
x <=> y = x ++ " = " ++ y

(@=) :: String -> [String] -> String
q @= xs = q <=> paren xs

(=@) :: [String] -> String -> String
qs =@ x = paren qs <=> x

