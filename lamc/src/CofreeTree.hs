{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module CofreeTree where

import Control.Comonad.Cofree
import Eval
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

num n = () :< ANumber n
var v = () :< AVar v
lex w = () :< ALex w
lam v b = () :< ALambda v b
app f x = () :< AApply f x

type CfTree = Cofree AST

fringe :: CfTree () -> String
fringe cf = fringePrec False cf ""
  where fringePrec :: Bool -> CfTree () -> ShowS
        fringePrec d term = case unwrap term of
          ANumber n -> showString (show n)
          AVar v -> showString v
          ALex w -> showString w
          AApply x y -> showParen d $ case unwrap x of
            ALambda v body -> fringePrec True x . showChar ' ' . fringePrec True y
            _              -> fringePrec False x . showChar ' ' . fringePrec True y
          ALambda v body -> showParen d $ 
            showString v . showString ". " . fringePrec False body

instance {-# OVERLAPPING #-} Show (CfTree String) where
  show = drawVerticalTree . cf2tree1
    where cf2tree1 (x :< f) = Node x $ case f of
            AApply fun arg -> [cf2tree1 fun, cf2tree1 arg]
            ALambda v body -> [Node ("\\" ++ v) [], cf2tree1 body]
            _ -> []

instance {-# OVERLAPPING #-} Show a => Show (CfTree a) where
  show = drawVerticalTree . cf2tree2
    where cf2tree2 (x :< f) = Node (show x) $ case f of
            AApply fun arg -> [cf2tree2 fun, cf2tree2 arg]
            ALambda v body -> [Node ("\\" ++ v) [], cf2tree2 body]
            _ -> []
