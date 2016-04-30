{-# LANGUAGE GADTs #-}

module HOAS where

import DbExp
import Bound

data ExprH a where
  NumH  :: Int                     -> ExprH a
  Lift  :: a                       -> ExprH a
  LamH  :: (ExprH a -> ExprH b)      -> ExprH (a -> b)
  AppH  :: ExprH (a -> b) -> ExprH a -> ExprH b
  FixH  :: ExprH (a -> a)           -> ExprH a

ident :: ExprH (a -> a)
ident = LamH id

tr :: ExprH (a -> b -> a)
tr = LamH (LamH . const)

fl :: ExprH (a -> b -> b)
fl = LamH (const ident)

eval :: ExprH a -> a
eval (Lift v)    = v
eval (LamH f)     = eval . f . Lift
eval (AppH e1 e2) = eval e1 (eval e2)
eval (FixH f)     = eval f (eval (FixH f))

fact :: ExprH (Integer -> Integer)
fact =
  FixH (
    LamH (\f ->
      LamH (\y ->
        Lift (
          if eval y == 0
          then 1
          else eval y * eval f (eval y - 1)))))

test :: Integer
test = eval fact 10

main :: IO ()
main = print test
