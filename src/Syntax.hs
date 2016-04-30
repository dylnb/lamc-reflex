{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as CF
import qualified Control.Comonad as CM
-- import Data.Tree
-- import Data.Tree.Pretty

type Name = String

data Expr a
  = Var Name
  | App a a
  | Lam Name a
  | Lit Lit
  -- | Let Name Expr Expr
  -- | If Expr Expr Expr
  -- | Fix Expr
  -- | Op Binop Expr Expr

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Program a = Program [Decl a] (Expr a) deriving Eq

type Decl a = (String, Expr a)

deriving instance Show a => Show (Expr a)
deriving instance Functor Expr
deriving instance Foldable Expr
deriving instance Traversable Expr
deriving instance Eq a => Eq (Expr a)
deriving instance Ord a => Ord (Expr a)

var v = () :< Var v
lit w = () :< Lit w
lam v b = () :< Lam v b
app f x = () :< App f x

type CfExpr = Cofree Expr

extract :: CfExpr a -> a
extract = CM.extract

unwrap :: CfExpr a -> Expr (CfExpr a)
unwrap = CF.unwrap


-- instance {-# OVERLAPPING #-} Show (CfTree String) where
--   show = drawVerticalTree . cf2tree1
--     where cf2tree1 (x :< f) = Node x $ case f of
--             AApply fun arg -> [cf2tree1 fun, cf2tree1 arg]
--             ALambda v body -> [Node ("\\" ++ v) [], cf2tree1 body]
--             _ -> []

-- instance {-# OVERLAPPING #-} Show a => Show (CfTree a) where
--   show = drawVerticalTree . cf2tree2
--     where cf2tree2 (x :< f) = Node (show x) $ case f of
--             AApply fun arg -> [cf2tree2 fun, cf2tree2 arg]
--             ALambda v body -> [Node ("\\" ++ v) [], cf2tree2 body]
--             _ -> []
