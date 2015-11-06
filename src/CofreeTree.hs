{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module CofreeTree where

import Control.Comonad.Cofree
import Data.Tree
import Data.Tree.Pretty

data AST a
  = ANumber Int
  | AVar String
  | ALex String
  | ALambda String a
  | AApply a a

deriving instance Show a => Show (AST a)
deriving instance Functor AST
deriving instance Foldable AST
deriving instance Traversable AST
deriving instance Eq a => Eq (AST a)
deriving instance Ord a => Ord (AST a)

newtype Mu f = Mu (f (Mu f))

num = Mu . ANumber
var = Mu . AVar
lex = Mu . ALex
lam = (Mu .) . ALambda
app = (Mu .) . AApply


type CfTree = Cofree AST
type MuTree = Mu AST

cf2tree1 :: CfTree String -> Tree String
cf2tree1 (x :< f) = Node x $
  case f of
    AApply fun arg -> [cf2tree1 fun, cf2tree1 arg]
    ALambda v body -> [Node ("λ" ++ v) [], cf2tree1 body]
    _ -> []
  
cf2tree2 :: Show a => CfTree a -> Tree String
cf2tree2 (x :< f) = Node (show x) $
  case f of
    AApply fun arg -> [cf2tree2 fun, cf2tree2 arg]
    ALambda v body -> [Node ("λ" ++ v) [], cf2tree2 body]
    _ -> []

showMe1 :: CfTree String -> IO ()
showMe1 = putStrLn . drawVerticalTree . cf2tree1

showMe2 :: Show a => CfTree a -> IO ()
showMe2 = putStrLn . drawVerticalTree . cf2tree2
