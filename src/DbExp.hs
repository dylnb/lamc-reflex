module DbExp where

import Data.List (elemIndex)
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

data Exp e a
  = N Int
  | V a
  | Exp e a :@ Exp e a
  | Lam (Scope (Name String ()) (Exp e) a)
  | Let [Scope (Name String Int) (Exp e) a] (Scope (Name String Int) (Exp e) a)
  | Ext e
  deriving (Eq,Ord,Show,Read)

lambda :: String -> Exp e String -> Exp e String
lambda u b = Lam (abstract1Name u b)

let_ :: [(String, Exp e String)] -> Exp e String -> Exp e String
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstractName (`elemIndex` map fst bs)

instance Functor (Exp e) where fmap       = fmapDefault
instance Foldable (Exp e) where foldMap    = foldMapDefault

-- deriving instance Functor (Exp e)

instance Applicative (Exp e) where
  pure  = V
  (<*>) = ap

instance Traversable (Exp e) where
  traverse f (N i)      = pure (N i)
  traverse f (V a)      = V <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)    = Lam <$> traverse f e
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b
  traverse f (Ext e)    = pure (Ext e)

instance Monad (Exp e) where
  return = V
  N i      >>= f = N i
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)
  Ext e    >>= f = Ext e

-- these 4 classes are needed to help Eq, Ord, Show and Read pass through Scope
instance Eq e => Eq1 (Exp e)               where (==#)      = (==)
instance (Eq e, Ord e) => Ord1 (Exp e)     where compare1   = compare
instance (Eq e, Show e) => Show1 (Exp e)   where showsPrec1 = showsPrec
instance (Eq e, Read e) => Read1 (Exp e)   where readsPrec1 = readsPrec

{--}
nf :: Exp e a -> Exp e a
nf e@N{}   = e
nf e@V{}   = e
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
nf (f :@ a) = case whnf f of
  Lam b -> nf (instantiate1Name a b)
  f' -> nf f' :@ nf a
nf (Let bs b) = nf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)
nf e@Ext{}   = e

{--}
whnf :: Exp e a -> Exp e a
whnf e@N{}   = e
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1Name a b)
  f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)
whnf e@Ext{}   = e

{--}
infixr 0 !
(!) :: String -> Exp e String -> Exp e String
x ! t = lambda x t

fromLex :: [(String, Exp e String)] -> Exp e String -> Exp e a
fromLex lex = fromJust . closed . let_ lex


-- TODO: use a real pretty printer

prettyPrec :: Show e => [String] -> Bool -> Int -> Exp e String -> ShowS
prettyPrec _      d n (N i)      = showString (show i)
prettyPrec _      d n (V a)      = showString a
prettyPrec vs     d n (x :@ y)   = showParen d $ 
  case x of
    (Lam b) -> prettyPrec vs True n x . showChar ' ' . prettyPrec vs True n y
    _       -> prettyPrec vs False n x . showChar ' ' . prettyPrec vs True n y
prettyPrec (v:vs) d n (Lam b)    = showParen d $ 
  showString u . showString ". " . prettyPrec vs False n (instantiate1Name (V u) b)
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
prettyPrec _      d n (Ext e) = showString (show e)

prettyWith :: Show e => [String] -> Exp e String -> String
prettyWith vs t = prettyPrec (filter (`notElem` toList t) vs) False 0 t ""

pretty :: Show e => Exp e String -> String
pretty = prettyWith $ [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

--}

