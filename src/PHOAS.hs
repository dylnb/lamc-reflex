{-# LANGUAGE RankNTypes #-}

module PHOAS where

import DbExp
import Bound

data ExprP e a
  = VarP a
  | AppP (ExprP e a) (ExprP e a)
  | LamP (a -> ExprP e a)
  | NumP Int
  | ExtP e

vp = VarP
lp = LamP
np = NumP
ep = ExtP
ap = AppP

newtype Expr e = Expr { unExpr :: forall a. ExprP e a }

db2phoas :: Exp e a -> ExprP e a
db2phoas (N n) = NumP n
db2phoas (V x) = VarP x
db2phoas (f :@ x) = AppP (db2phoas f) (db2phoas x)
db2phoas (Lam b) = LamP (\x -> db2phoas $ instantiate1 (V x) b)
db2phoas (Ext e) = ExtP e
