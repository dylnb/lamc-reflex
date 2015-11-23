{-# LANGUAGE StandaloneDeriving #-}

module TypeCheck where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import Data.Foldable (Foldable, fold)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable (Traversable, sequence)
import qualified Data.Map as M
import CofreeTree


data Type
  = TNumber
  | TVar Int
  | TLambda Type Type

instance Show Type where
  show TNumber = "N"
  show (TVar i) = "V" ++ show i
  show (TLambda a b) = case a of
    TLambda _ _ -> "(" ++ show a ++ ")" ++ " -> " ++ show b
    _ -> show a ++ " -> " ++ show b

data Const = EqConst Type Type

deriving instance Show Const

data TypeResult
  = TypeResult { constraints :: [Const], assumptions :: M.Map String [Type] }

deriving instance Show TypeResult

instance Monoid TypeResult where
  mempty = TypeResult mempty mempty
  mappend a b =
    TypeResult (constraints a `mappend` constraints b)
               (assumptions a `mappend` assumptions b)

data TypeState t m = TypeState { varId :: Int, memo :: M.Map t m }
type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

freshVarId :: State (TypeState t m) Type
freshVarId = do
  v <- gets varId
  modify $ \s -> s { varId = succ v }
  return $ TVar v

memTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memTC f c = gets memo >>= maybe memoize return . M.lookup c
  where memoize = do
          r <- f c
          modify $ \s -> s { memo = M.insert c r $ memo s }
          return r

genConsts :: Cofree AST () -> TypeCheck (Cofree AST ())
genConsts (() :< ANumber _) = return (TNumber, mempty)
genConsts (() :< ALex _) = return (TVar 1, mempty) -- needs to written!
genConsts (() :< AVar s) =
  freshVarId >>= \var -> return (var, TypeResult [] (M.singleton s [var]))
genConsts (() :< ALambda s b) = do
  var <- freshVarId
  br <- memTC genConsts b
  let cs = maybe [] (map $ EqConst var) (M.lookup s . assumptions $ snd br)
      as = M.delete s . assumptions $ snd br
  return (TLambda var (fst br), TypeResult (constraints (snd br) `mappend` cs) as)
genConsts (() :< AApply a b) = do
  var <- freshVarId
  ar <- memTC genConsts a
  br <- memTC genConsts b
  let cs = TypeResult [EqConst (fst ar) $ TLambda (fst br) var] mempty
  return (var, snd ar `mappend` snd br `mappend` cs)

solveConsts :: [Const] -> Maybe (M.Map Int Type)
solveConsts = foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
  where solve maybeSubs (EqConst a b) =
          maybeSubs >>= \ss -> unify (subst ss a) (subst ss b)

unify :: Type -> Type -> Maybe (M.Map Int Type)
unify (TVar i) b = Just $ M.singleton i b
unify a (TVar i) = Just $ M.singleton i a
unify TNumber TNumber = Just M.empty
unify (TLambda a b) (TLambda c d) = do
  s1 <- unify a c
  liftM2 mappend (unify (subst s1 b) (subst s1 d)) $ Just s1
unify _ _ = Nothing

subst :: M.Map Int Type -> Type -> Type
subst ss v@(TVar i) =
  case M.lookup i ss of
    Just (TVar j) -> if i == j then v else subst ss (TVar j)
    Just t -> subst ss t
    Nothing -> v
subst ss (TLambda a b) = TLambda (subst ss a) (subst ss b)
subst _ t = t
