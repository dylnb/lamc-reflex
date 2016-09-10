module DbExp where

import Data.List (elemIndex, last)
import Data.Foldable hiding (notElem)
import Data.Maybe (fromJust)
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude hiding (foldr,abs)
import Prelude.Extras
import Bound
import Bound.Name
import Bound.Scope (bindings)
import System.Exit

infixl 9 :@

data L
  = LI Integer
  | LB Bool
  deriving (Eq, Ord, Read)

instance Show L where
  show (LI n) = show n
  show (LB b) = show b

data Exp a
  = L L
  | V a
  | Exp a :@ Exp a
  | Lam (Scope (Name String ()) Exp a)
  | Let [Scope (Name String Int) Exp a] (Scope (Name String Int) Exp a)
  deriving (Eq,Ord,Show,Read)

lambda :: String -> Exp String -> Exp String
lambda u b = Lam (abstract1Name u b)

let_ :: [(String, Exp String)] -> Exp String -> Exp String
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstractName (`elemIndex` map fst bs)

instance Functor Exp  where fmap       = fmapDefault
instance Foldable Exp where foldMap    = foldMapDefault

-- deriving instance Functor Exp

instance Applicative Exp where
  pure  = V
  (<*>) = ap

instance Traversable Exp where
  traverse f (L l)      = pure (L l)
  traverse f (V a)      = V <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)    = Lam <$> traverse f e
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b

instance Monad Exp where
  return = V
  L l      >>= f = L l
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)

-- these 4 classes are needed to help Eq, Ord, Show and Read pass through Scope
instance Eq1 Exp      where (==#)      = (==)
instance Ord1 Exp     where compare1   = compare
instance Show1 Exp    where showsPrec1 = showsPrec
instance Read1 Exp    where readsPrec1 = readsPrec

csubst :: Monad m => (Exp a -> m ()) -> Exp a -> Exp a -> m (Exp a)
csubst k a f@(Lam b) = k (f :@ a) >> return (instantiate1Name a b)

{--}
nf :: Exp a -> Exp a
nf e@L{}   = e
nf e@V{}   = e
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
nf (f :@ a) = case whnf f of
  Lam b -> nf (instantiate1Name a b)
  f' -> nf f' :@ nf a
nf (Let bs b) = nf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

{--}
nfTrace :: Monad m => (Exp a -> m ()) -> Exp a -> m (Exp a)
nfTrace k e@L{}   = return e
nfTrace k e@V{}   = return e
nfTrace k (Lam b) = (Lam . toScope) <$> nfTrace (k . Lam . toScope) (fromScope b)
nfTrace k (f :@ a) =
  let tracedF = whnfTrace (k . (:@ a)) f
      test = \exp -> case exp of
        Lam b -> csubst k a (Lam b) >>= nfTrace k
        f' -> liftM2 (:@) (nfTrace k f') (nfTrace k a)
  in tracedF >>= test
-- nfTrace (Let bs b) = nf (inst b)
--   where es = map inst bs
--         inst = instantiateName (es !!)

  
{--}
whnf :: Exp a -> Exp a
whnf e@L{}   = e
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1Name a b)
  f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

whnfTrace :: Monad m => (Exp a -> m ()) -> Exp a -> m (Exp a)
whnfTrace k e@L{}   = return e
whnfTrace k e@V{}   = return e
whnfTrace k e@Lam{} = return e
whnfTrace k (f :@ a) =
  let tracedF = whnfTrace (k . (:@ a)) f
      test = \exp -> case exp of
        Lam b -> csubst k a (Lam b) >>= whnfTrace k 
        f'    -> return (f' :@ a)
  in tracedF >>= test
-- whnfSteps (Let bs b) = whnf (inst b)
--   where es = map inst bs
--         inst = instantiateName (es !!)

{--}
infixr 0 !
(!) :: String -> Exp String -> Exp String
x ! t = lambda x t

fromLex :: [(String, Exp String)] -> Exp String -> Exp a
fromLex lexicon = fromJust . closed . let_ lexicon


-- TODO: use a real pretty printer

prettyPrec :: [String] -> Bool -> Int -> Exp String -> ShowS
prettyPrec _      d n (L l)      = showString (show l)
prettyPrec _      d n (V a)      = showString a
prettyPrec vs     d n (x :@ y)   = showParen d $ 
  case x of
    (Lam b) -> prettyPrec vs True n x . showChar ' ' . prettyPrec vs True n y
    _       -> prettyPrec vs False n x . showChar ' ' . prettyPrec vs True n y
prettyPrec (v:vs) d n (Lam b)    = showParen d $ 
  showString "\\" . showString u . showString ". " . prettyPrec vs False n (instantiate1Name (V u) b)
  where u = case bindings b of
              [] -> v
              l  -> name $ head l
prettyPrec vs     d n (Let bs b) = showParen d $ 
  showString "let" .  foldr (.) id (zipWith showBinding xs bs) .
  showString " in " . indent . prettyPrec ys False n (inst b)
  where (xs,ys) = splitAt (length bs) vs
        inst = instantiateName (\n -> V (xs !! n))
        indent = showString ('\n' : replicate (n + 4) ' ')
        showBinding x b = indent . showString x . showString " = " . prettyPrec ys False (n + 4) (inst b)

prettyWith :: [String] -> Exp String -> String
prettyWith vs t = prettyPrec (filter (`notElem` toList t) vs) False 0 t ""

pretty :: Exp String -> String
pretty = prettyWith $ [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

--}

