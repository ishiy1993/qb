module QB.Exp.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Data.Void
import qualified Data.Map.Strict as M
import qualified Data.Scientific as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import QB.Exp

data ParserConfig = ParserConfig
  { cfgAxes :: [String]
  , cfgVariables :: [Ident]
  }

type Parser = ReaderT ParserConfig (Parsec Void String)

parseWith :: [String] -> [Ident] -> Parser a -> String -> String -> Either (ParseErrorBundle String Void) a
parseWith as vs p = parse $ runReaderT p (ParserConfig as vs)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

imm :: Parser Double
imm = lexeme (S.toRealFloat <$> L.scientific)

identifier :: Parser Ident
identifier = lexeme p
  where p = (:) <$> letterChar <*> many alphaNumChar

eqsParser :: Parser Equations
eqsParser = between sc eof eqs
  where eqs = sepEndBy eqParser sc

eqParser :: Parser Equation
eqParser = do
  l <- expParser
  void $ symbol "="
  r <- expParser
  return (Equation l r)

expParser :: Parser Exp
expParser = makeExprParser term ops
  where
    ops :: [[Operator Parser Exp]]
    ops = [ [InfixL (Pow <$ symbol "**")]
          , [Prefix (Neg <$ symbol "-")]
          , [InfixL (Mul <$ symbol "*")
            ,InfixL (Div <$ symbol "/")
            ]
          , [InfixL (Add <$ symbol "+")
            ,InfixL (Sub <$ symbol "-")
            ]
          ]

    term :: Parser Exp
    term = parens expParser
           <|> symParser
           <|> Imm <$> imm

symParser :: Parser Exp
symParser = lexeme $ do
  vs <- reader cfgVariables
  n <- identifier
  ds <- option M.empty $ coordsParser
  return $ Sym n (if n `elem` vs then Variable else Constant) ds

coordsParser :: Parser Coords
coordsParser = do
  axes <- reader cfgAxes
  let coord = choice [string i *> pure c | (i,c) <- zip axes [I0 .. I3]] 
  build <$> (char '_' *> some coord)


-- Sample
euler1 :: Equations
euler1 = case parseWith ["t","x"] ["r","u","p"] eqsParser "" eom of
           Right eqs -> eqs
           Left _ -> error "Error in euler1"
  where
    eom = unlines [ "r_t = -(u*r_x + r*u_x)"
                  , "u_t = -u*u_x - p_x/r"
                  , "p_t = -u*p_x - gm*p*u_x"
                  ]

euler2 :: Equations
euler2 = case parseWith ["t","x","y"] ["r","u","v","p"] eqsParser "" eom of
           Right eqs -> eqs
           Left _ -> error "Error in euler2"
  where
    eom = unlines [ "r_t = -u*r_x - v*r_y - r*(u_x + v_y)"
                  , "u_t = -u*u_x - v*u_y - p_x/r"
                  , "v_t = -u*v_x - v*v_y - p_y/r"
                  , "p_t = -u*p_x - v*p_y - gm*p*(u_x + v_y)"
                  ]
