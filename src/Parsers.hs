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
