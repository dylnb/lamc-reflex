module Eval where

import DbExp
import Data.Maybe (fromJust)
import Bound

data Val
  = VInt Int
  | VFun (Val -> Val)

instance Show Val where
  show (VInt i) = show i
  show (VFun _) = "fun"

type Env = [(String,Val)]

update :: Env -> Val -> (Env, String)
update env v = ((freshvar,v) : env, freshvar)
  where vars = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
        freshvar = head (filter (`notElem` map fst env) vars)

eval :: Env -> Exp String -> Val
eval env exp =
  let t = nf exp in
  case t of
    N i -> VInt i
    V x -> fromJust $ lookup x env
    Lam e -> VFun $ \v ->
               let env' = update env v in
               eval (fst env') (instantiate1 (V $ snd env') e)
    m :@ n -> case eval env m of {VFun f -> f (eval env n)}
