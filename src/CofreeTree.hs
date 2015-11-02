{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module CofreeTree where

import Control.Comonad.Cofree

data AST a
  = ALambda String a
  | AApply a a
  | ANumber Int
  | AString String
  | AIdent String

deriving instance Show a => Show (AST a)
deriving instance Functor AST
deriving instance Foldable AST
deriving instance Traversable AST
deriving instance Eq a => Eq (AST a)
deriving instance Ord a => Ord (AST a)

newtype Mu f = Mu (f (Mu f))

num = Mu . ANumber
var = Mu . AIdent
lam = (Mu .) . ALambda
app = (Mu .) . AApply


type CfTree = Cofree AST
type MuTree = Mu AST
