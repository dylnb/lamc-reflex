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

cf2tree :: Show a => CfTree a -> Tree String
cf2tree (x :< f) = Node (show x) $
  case f of
    AApply fun arg -> [cf2tree fun, cf2tree arg]
    ALambda v body -> [Node ("λ" ++ v) [], cf2tree body]
    _ -> []

showMe :: Show a => CfTree a -> IO ()
showMe = putStrLn . drawVerticalTree . cf2tree
