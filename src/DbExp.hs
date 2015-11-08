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

data Exp a
  = N Int
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
  traverse f (N i)      = pure (N i)
  traverse f (V a)      = V <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)    = Lam <$> traverse f e
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b

instance Monad Exp where
  return = V
  N i      >>= f = N i
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)

-- these 4 classes are needed to help Eq, Ord, Show and Read pass through Scope
instance Eq1 Exp      where (==#)      = (==)
instance Ord1 Exp     where compare1   = compare
instance Show1 Exp    where showsPrec1 = showsPrec
instance Read1 Exp    where readsPrec1 = readsPrec

{--}
nf :: Exp a -> Exp a
nf e@N{}   = e
nf e@V{}   = e
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
nf (f :@ a) = case whnf f of
  Lam b -> nf (instantiate1Name a b)
  f' -> nf f' :@ nf a
nf (Let bs b) = nf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

{--}
whnf :: Exp a -> Exp a
whnf e@N{}   = e
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1Name a b)
  f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

{--}
infixr 0 !
(!) :: String -> Exp String -> Exp String
x ! t = lambda x t

true :: Exp String
true = "y" ! "n" ! V"y"

fromLex :: Exp String -> Exp a
fromLex exp = fromJust $ closed $ let_
  [ ("false",  "y" ! "n" ! V"n")
  , ("true",   "y" ! "n" ! V"y")
  , ("if",     "b" ! "y" ! "n" ! V"b" :@ V"y" :@ V"n")
  , ("and",    "u" ! "v" ! V"u" :@ V"v" :@ V"false")
  , ("or",     "u" ! "v" ! V"u" :@ V"true" :@ V"v")
  , ("not",    "u" ! "y" ! "n" ! V"u" :@ V"n" :@ V"y")
  , ("zero",   "s" ! "z" ! V"z")
  , ("succ",   "n" ! "s" ! "z" ! V"s" :@ (V"n" :@ V"s" :@ V"z"))
  , ("pred",   "n" ! V"fst" :@ (V"n" :@ ("p" ! V"pair" :@ (V"snd" :@ V"p") :@ (V"add" :@ V"one" :@ (V"snd" :@ V"p"))) :@ (V"pair" :@ V"zero" :@ V"zero")))
  , ("one",    V"succ" :@ V"zero")
  , ("two",    V"succ" :@ V"one")
  , ("three",  V"succ" :@ V"two")
  , ("isZero", "n" ! V"n" :@ ("m" ! V"false") :@ V"true")
  , ("const",  "x" ! "y" ! V"x")
  , ("pair",   "a" ! "b" ! "k" ! V"k" :@ V"a" :@ V"b")
  , ("fst",    "ab" ! V"ab" :@ ("a" ! "b" ! V"a"))
  , ("snd",    "ab" ! V"ab" :@ ("a" ! "b" ! V"b"))
  -- we have a lambda calculus extended with recursive bindings, so we don't need to use fix
  , ("add",    "m" ! "n" ! "s" ! "z" ! V"m" :@ V"s" :@ (V"n" :@ V"s" :@ V"z"))
  , ("sub",    "m" ! "n" ! V"m" :@ V"pred" :@ V"n")
  , ("mul",    "m" ! "n" ! "s" ! "z" ! V"m" :@ (V"n" :@ V"s") :@ V"z")
  , ("fac",    "m" ! "s" ! V"m" :@ ("x" ! "n" ! V"n" :@ (V"x" :@ ("a" ! "b" ! V"n" :@ V"a" :@ (V"a" :@ V"b")))) :@ ("y" ! V"s") :@ ("w" ! V"w"))
  , ("sumto",  "m" ! V"fst" :@ (V"m" :@ ("p" ! V"pair" :@ (V"add" :@ V"one" :@ (V"add" :@ (V"fst" :@ V"p") :@ (V"snd" :@ V"p"))) :@ (V"add" :@ V"one" :@ (V"snd" :@ V"p"))) :@ (V"pair" :@ V"zero" :@ V"zero")))
  , ("equal",  "m" ! "n" ! V"and" :@ (V"isZero" :@ (V"sub" :@ V"m" :@ V"n")) :@ (V"isZero" :@ (V"sub" :@ V"n" :@ V"m")))
  , ("n5",     V"add" :@ V"two" :@ V"three")
  , ("n6",     V"add" :@ V"three" :@ V"three")
  , ("n17",    V"add" :@ V"n6" :@ (V"add" :@ V"n6" :@ V"n5"))
  , ("n37",    V"succ" :@ (V"mul" :@ V"n6" :@ V"n6"))
  , ("n703",   V"sumto" :@ V"n37")
  , ("n720",   V"fac" :@ V"n6")
  ] exp

-- TODO: use a real pretty printer

prettyPrec :: [String] -> Bool -> Int -> Exp String -> ShowS
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

prettyWith :: [String] -> Exp String -> String
prettyWith vs t = prettyPrec (filter (`notElem` toList t) vs) False 0 t ""

pretty :: Exp String -> String
pretty = prettyWith $ [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

--}

