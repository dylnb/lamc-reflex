{-# LANGUAGE FlexibleContexts #-}

module Main where

import CofreeTree as CF
import DbExp
import TypeCheck
import Eval
import Parsers
import Lexicon

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Trans.Class
import System.Console.Haskeline


cf2db :: CfTree a -> Exp Prim String
cf2db cf =
  case unwrap cf of
    ANumber i -> N i
    AVar x -> V x
    ALex w -> fromLex arith (V w)
    AApply m n -> cf2db m :@ cf2db n
    ALambda x body -> x ! cf2db body

labelTypes :: CfTree () -> IO ()
labelTypes t = maybe (return ()) print $
  fmap (\ss -> fmap (subst ss . fst) r) maybeSubs
  where r = evalState (sequence $ labelConstraints t) (TypeState 0 M.empty)
        maybeSubs = solveConsts . constraints $ snd (extract r)

labelConstraints :: CfTree () -> CfTree (TypeCheck (CfTree ()))
labelConstraints = extend (memTC genConsts)

labelNormals :: CfTree () -> IO ()
labelNormals = print . extend (pretty . nf . cf2db)

labelMeanings :: CfTree () -> IO ()
labelMeanings = print . extend (eval interp [] . cf2db)
  -- where defEnv =
  --         [ ("id", VFun id)
  --         , ("plus1", VFun $ \(VInt i) -> VInt (i+1))
  --         , ("k1", VFun $ \(VFun g) -> g (VInt 1))
  --         , ("k2k5", VFun $ \(VFun g) ->
  --                       VFun $ \(VFun h) ->
  --                         let VInt i = g (VInt 2)
  --                             VInt j = h (VInt 5) in
  --                         VInt $ i + j)
  --         , ("comp", VFun $ \(VFun f) ->
  --                      VFun $ \(VFun g) ->
  --                        VFun $ \vx -> f (g vx))
  --         ]

t1 = app (var "k1") (var "plus1")
t2 = app (app (var "k2k5") (lam "x" (var "x"))) (lam "y" (var "z"))
add23 = app (app (CF.lex "add") (CF.lex "two")) (CF.lex "three")
iftt = app (app (app (CF.lex "if") (CF.lex "true")) (num 4)) (num 7)
comp = lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x")))))
k3 = lam "k" (num 3)
ident = lam "x" (var "x")
ex2 = app (app (app comp k3) ident) (num 7)
ex3 = app (lam "f" (app (var "f") (var "f"))) (lam "x" (var "x"))
ex4 = app (var "x") (var "x")
test = app (lam "b" (app (app (CF.lex "if") (var "b")) (CF.lex "true"))) (CF.lex "false")  

acc :: (Int -> Maybe String) -> CfTree () -> IO ()
acc g cf = runInputT defaultSettings loop
  where loop = do let opts = enumerate g cf
                  outputStrLn $ show opts
                  Just x <- getInputLine "What next? "
                  let (a :@ b) = cf2db . fromJust $ nthNode (read x) opts
                  outputStrLn $ pretty (nf a :@ nf b)
                  tryIt (read x) (a :@ b)
        tryIt ind gold = do
          Just guess <- getInputLine "Reduce: "
          case parseExp guess of
            Left s -> outputStrLn (show s)
            Right p -> do
              let correct = p == nf gold
              outputStrLn (show correct)
              if correct
                then do Just s <- getInputLine "Continue? "
                        let g' n = if n == ind then Just (pretty p) else g n
                        unless (s == "no") $ liftIO (acc g' cf)
                else tryIt ind gold


enumerate :: (Int -> Maybe String) -> CfTree () -> CfTree (Either Int String)
enumerate g cf = evalState (sequence $ extend enum cf) 0
  where enum :: CfTree () -> State Int (Either Int String)
        enum (() :< ANumber i) = state $ \s -> (Right $ "n" ++ show i, s+1)
        enum (() :< AVar x) = state $ \s -> (Right x, s+1)
        enum (() :< ALex w) = state $ \s ->
          (Right $ pretty . nf $ fromLex arith (V w), s+1)
        enum (() :< ALambda x b) = state $ \s ->
          case g (s+1) of
            Nothing -> (Left s, s+1)
            Just t  -> (Right $ pretty . nf $ x ! cf2db b, s+1)
        enum (() :< AApply a b) = state $ \s ->
          case g s of                          
            Nothing -> (Left s, s+1)           
            Just t  -> (Right t, s+1)          

nthNode :: Int -> CfTree a -> Maybe (CfTree a)
nthNode i cf = case iter cf 0 of {Left a -> Just a; Right _ -> Nothing}
  where iter cf n
          | n == i    = Left cf
          | otherwise = case cf of
                          (_ :< AApply a b) -> iter a (n+1) >>= iter b
                          (_ :< ALambda v b) -> iter b (n+1)
                          _ -> return n

main :: IO ()
main = acc (const Nothing) test 
