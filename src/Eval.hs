module Eval where

import DbExp
import Data.Maybe (fromMaybe, fromJust)
import Bound
import Lexicon
import PHOAS

data Val
  = VInt Int
  | VFun (Val -> Val)
  | VString String

instance Show Val where
  show (VInt i) = show i
  show (VFun _) = "fun"
  show (VString s) = s

type Env = [(String,Val)]

update :: Env -> Val -> (Env, String)
update env v = ((freshvar,v) : env, freshvar)
  where vars = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
        freshvar = head (filter (`notElem` map fst env) vars)

eval :: (Prim -> Val) -> Env -> Exp Prim String -> Val
eval i env exp =
  let t = nf exp in
  case t of
    N i -> VInt i
    V x -> fromMaybe (VString "???") (lookup x env)
    Lam e -> VFun $ \v ->
               let env' = update env v in
               eval i (fst env') (instantiate1 (V $ snd env') e)
    m :@ n -> case eval i env m of
                VFun f -> f (eval i env n)
                VString x -> VString x
    Ext e -> i e

interp :: Prim -> Val
interp Plus = VFun (\(VInt n) -> VFun (\(VInt m) -> VInt $ n + m))
interp Times = VFun (\(VInt n) -> VFun (\(VInt m) -> VInt $ n * m))

data Value
  = VL Int
  | VF (Value -> Value)

evalP :: (e -> Value) -> Expr e -> Value
evalP i e = ev i (unExpr e)
  where ev i (NumP n) = VL n
        ev i (VarP v) = v
        ev i (AppP exprf exprx) = let (VF f) = ev i exprf in f (ev i exprx)
        ev i (LamP exprf) = VF $ ev i . exprf
        ev i (ExtP e) = i e

interP :: Prim -> Value
interP Plus = VF (\(VL n) -> VF (\(VL m) -> VL $ n + m))
interP Times = VF (\(VL n) -> VF (\(VL m) -> VL $ n * m))

compP = LamP (\f -> LamP (\g -> LamP (AppP (VarP f) . AppP (VarP g) . VarP)))
sccP = LamP (\n -> AppP (AppP (ExtP Plus) (VarP n)) (NumP 1))

scc'P = db2phoas . fromJust . closed $ "n" ! Ext Plus :@ V"n" :@ N 1
