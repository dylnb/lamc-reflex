{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser (CfExpr ())
variable = do
  x <- identifier
  return (var x)

number :: Parser (CfExpr ())
number = do
  n <- integer
  return (lit (LInt (fromIntegral n)))

bool :: Parser (CfExpr ())
bool = (reserved "True" >> return (lit (LBool True)))
    <|> (reserved "False" >> return (lit (LBool False)))

-- fix :: Parser Expr
-- fix = do
--   reservedOp "fix"
--   x <- aexp
--   return (Fix x)

lambda :: Parser (CfExpr ())
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr lam body args

-- letin :: Parser Expr
-- letin = do
--   reserved "let"
--   x <- identifier
--   reservedOp "="
--   e1 <- expr
--   reserved "in"
--   e2 <- expr
--   return (Let x e1 e2)

-- letrecin :: Parser Expr
-- letrecin = do
--   reserved "let"
--   reserved "rec"
--   x <- identifier
--   reservedOp "="
--   e1 <- expr
--   reserved "in"
--   e2 <- expr
--   return (Let x e1 e2)

-- ifthen :: Parser Expr
-- ifthen = do
--   reserved "if"
--   cond <- aexp
--   reservedOp "then"
--   tr <- aexp
--   reserved "else"
--   fl <- aexp
--   return (If cond tr fl)

aexp :: Parser (CfExpr ())
aexp =
      parens expr
  <|> bool
  <|> number
  -- <|> ifthen
  -- <|> fix
  -- <|> try letrecin
  -- <|> letin
  <|> lambda
  <|> variable

term :: Parser (CfExpr ())
term = Ex.buildExpressionParser table aexp

-- infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
-- infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators (CfExpr ())
table = [
    -- [
    --   infixOp "*" (Op Mul) Ex.AssocLeft
    -- ],
    -- [
    --   infixOp "+" (Op Add) Ex.AssocLeft
    -- , infixOp "-" (Op Sub) Ex.AssocLeft
    -- ],
    -- [
    --   infixOp "==" (Op Eql) Ex.AssocLeft
    -- ]
  ]

expr :: Parser (CfExpr ())
expr = do
  es <- many1 term
  return (foldl1 app es)

type Binding = (String, CfExpr ())

-- letdecl :: Parser Binding
-- letdecl = do
--   reserved "let"
--   name <- identifier
--   args <- many identifier
--   reservedOp "="
--   body <- expr
--   return $ (name, foldr Lam body args)

-- letrecdecl :: Parser (String, Expr)
-- letrecdecl = do
--   reserved "let"
--   reserved "rec"
--   name <- identifier
--   args <- many identifier
--   reservedOp "="
--   body <- expr
--   return $ (name, Fix $ foldr Lam body (name:args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
-- decl = try letrecdecl <|> letdecl <|> val
decl = val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError (CfExpr ())
parseExpr input = parse (contents expr) "<stdin>" input

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, CfExpr ())]
parseModule fname input = parse (contents modl) fname input
