module Parsers where

import DbExp
import CofreeTree
import Text.Parsec


type ExpParser = Parsec String () (Exp String)

plam = char '\\'
pvar = return <$> letter
pdot = (char '.' >> spaces) <|> skipMany1 space
parens = between (char '(') (char ')')

parseVar :: ExpParser
parseVar = V <$> pvar

parseAbs :: ExpParser
parseAbs = plam >> pvar >>= \v -> pdot >> parseTerm >>= \tm -> return (lambda v tm)

parseNonApp :: ExpParser
parseNonApp = parens parseTerm <|> parseAbs <|> parseVar

parseTerm :: ExpParser
parseTerm = chainl1 parseNonApp (space >> return (:@))

parseExp :: String -> Either ParseError (Exp String)
parseExp = parse parseTerm "λ-calculus"

type CfParser = Parsec String () (CfTree ())

cflam = char '\\'
cfvar = return <$> letter
cfdot = (char '.' >> spaces) <|> skipMany1 space
cfparens = between (char '(') (char ')')

parseCfVar :: CfParser
parseCfVar = var <$> cfvar

parseCfAbs :: CfParser
parseCfAbs = cflam >> cfvar >>= \v -> cfdot >> parseCfTerm >>= \tm -> return (lam v tm)

parseCfNonApp :: CfParser
parseCfNonApp = cfparens parseCfTerm <|> parseCfAbs <|> parseCfVar

parseCfTerm :: CfParser
parseCfTerm = chainl1 parseCfNonApp (space >> return app)

parseCf :: String -> Either ParseError (CfTree ())
parseCf = parse parseCfTerm "λ-calculus"

