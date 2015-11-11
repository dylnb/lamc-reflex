module Parsers where

import DbExp
import CofreeTree
import Text.ParserCombinators.Parsec

dbterm = dblam

dblam :: GenParser Char st (Exp String)
dblam = char '\\' >> letter >>= \x -> char '.' >> spaces >> dbterm >>= \tm -> return (lambda [x] tm)
        <|> (string "BODY" >> return (N 5))
