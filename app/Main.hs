module Main where

import CofreeTree
import DB
import System.Exit
import Control.Comonad
import Control.Comonad.Cofree


cofreeDB :: Cofree AST a -> Exp String
cofreeDB (_ :< f) =
  case f of
    (ANumber i) -> N i
    (AIdent x) -> V x
    (AApply m n) -> cofreeDB m :@ cofreeDB n
    (ALambda x body) -> lambda x (cofreeDB body)

annotate :: Cofree AST () -> Cofree AST String
annotate = extend (pretty . nf . cofreeDB)

evaluate :: Cofree AST () -> Cofree AST Val
evaluate = extend (eval defEnv . cofreeDB)

-- attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
-- attribute c =
--     let initial = TypeState { memo = M.empty, varId = 0 }
--     in evalState (sequence $ extend (memoizedTC generateConstraints) c) initial

{--}

-- example2 :: Mu AST
-- example2 = Mu $ AApply (Mu . ALambda "x" . Mu $ AIdent "x") (Mu $ ANumber 2)

t :: Mu AST
t = app (var "k1") (var "plus1")

comp :: Mu AST
comp = lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x")))))

k3 :: Mu AST
k3 = lam "k" (num 3)

ident :: Mu AST
ident = lam "x" (var "x")

ex2 :: Mu AST
ex2 = app (app (app comp k3) ident) (num 7)

ex3 :: Mu AST
ex3 = app (lam "f" (app (var "f") (var "f"))) (lam "x" (var "x"))

ex4 :: Mu AST
ex4 = app (var "x") (var "x")

demo :: Mu AST -> IO ()
demo ex = do
  print $ cofreeMu ex
  putStrLn ""
  print . attribute $ cofreeMu ex
  putStrLn ""
  print . typeTree $ cofreeMu ex

{--
 
main :: IO ()
main = demo ex2

--}

{--}
pp :: Exp String -> IO ()
pp = putStrLn . pretty

main = do
  pp (muDB ident)
  pp (muDB ex3)
  pp (muDB comp)
  -- pp cooked
  -- let result = nf cooked
  -- if result == true
  --   then putStrLn "Result correct."
  --   else do
  --     putStrLn "Unexpected result:"
  --     pp result
  --     exitFailure

--}
