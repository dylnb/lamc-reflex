module Parsers where

import DbExp
import CofreeTree
import Text.Parsec


type ExpParser e = Parsec String () (Exp e String)

plam = char '\\'
pvar = return <$> letter
pdot = (char '.' >> spaces) <|> skipMany1 space
parens = between (char '(') (char ')')

parseVar :: ExpParser e
parseVar = V <$> pvar

parseAbs :: ExpParser e
parseAbs = plam >> pvar >>= \v -> pdot >> parseTerm >>= \tm -> return (lambda v tm)

parseNonApp :: ExpParser e
parseNonApp = parens parseTerm <|> parseAbs <|> parseVar

parseTerm :: ExpParser e
parseTerm = chainl1 parseNonApp (space >> return (:@))

parseExp :: String -> Either ParseError (Exp e String)
parseExp = parse parseTerm "λ-calculus"
