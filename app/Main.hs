module Main where

import CofreeTree as CF
import DbExp
import TypeCheck
import Eval
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

mu2cf :: MuTree -> CfTree ()
mu2cf (Mu f) = () :< fmap mu2cf f


cf2db :: CfTree a -> Exp String
cf2db cf =
  case unwrap cf of
    (ANumber i) -> N i
    (AVar x) -> V x
    (ALex w) -> fromLex (V w)
    (AApply m n) -> cf2db m :@ cf2db n
    (ALambda x body) -> x ! cf2db body

mu2db :: MuTree -> Exp String
mu2db = cf2db . mu2cf

labelTypes :: MuTree -> IO ()
labelTypes t = maybe (return ()) showMe2 $
  fmap (\subs -> fmap (substitute subs . fst) r) maybeSubs
  where defTypeState = TypeState { memo = M.empty, varId = 0 }
        r = evalState (sequence $ labelConstraints t) defTypeState
        maybeSubs = solveConstraints . constraints $ snd (extract r)

labelConstraints :: MuTree -> CfTree (TypeCheck (CfTree ()))
labelConstraints = extend (memoizedTC generateConstraints) . mu2cf

labelNormals :: MuTree -> IO ()
labelNormals = showMe1 . extend (pretty . nf . cf2db) . mu2cf

labelMeanings :: MuTree -> IO ()
labelMeanings = showMe2 . extend (eval defEnv . cf2db) . mu2cf
  where defEnv =
          [ ("id", VFun id)
          , ("plus1", VFun $ \(VInt i) -> VInt (i+1))
          , ("k1", VFun $ \(VFun g) -> g (VInt 1))
          , ("k2k5", VFun $ \(VFun g) ->
                        VFun $ \(VFun h) ->
                          let VInt i = g (VInt 2)
                              VInt j = h (VInt 5) in
                          VInt $ i + j)
          ]

t1 :: MuTree
t1 = app (var "k1") (var "plus1")

t2 :: MuTree
t2 = app (app (var "k2k5") (lam "x" (var "x"))) (lam "y" (var "z"))

-- recursive; blows up!
add23 :: MuTree
add23 = app (app (CF.lex "add") (CF.lex "two")) (CF.lex "three")

iftt :: MuTree
iftt = app (app (CF.lex "if") (CF.lex "True")) (CF.lex "True")

comp :: MuTree
comp = lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x")))))

k3 :: MuTree
k3 = lam "k" (num 3)

ident :: MuTree
ident = lam "x" (var "x")

ex2 :: MuTree
ex2 = app (app (app comp k3) ident) (num 7)

ex3 :: MuTree
ex3 = app (lam "f" (app (var "f") (var "f"))) (lam "x" (var "x"))

ex4 :: MuTree
ex4 = app (var "x") (var "x")

main :: IO ()
main = return ()

{--
pp :: Exp String -> IO ()
pp = putStrLn . pretty

main = do
  pp (mu2db ident)
  pp (mu2db ex3)
  pp (mu2db comp)

--}
