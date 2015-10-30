How do we add extra information to a tree? This has been called [The
AST Typing
Problem](http://blog.ezyang.com/2013/05/the-ast-typing-problem/).

After being hit with this problem in Roy's new type-inference engine,
I tried figuring out how to represent the algorithm. I eventually
realised that it looked like a comonadic operation. Turns out it's
been done before but I couldn't find any complete example.

Below is some literate Haskell to show how to use the Cofree Comonad
to perform [bottom-up, constraint-based
type-inference](http://igitur-archive.library.uu.nl/math/2007-1122-200535/heeren_02_generalizinghindleymilner.pdf). It's
essentially a tiny version of how Roy's new type-system works.

> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE StandaloneDeriving #-}
>
> module CofreeTree where
>
> import Prelude hiding (sequence)
>
> import Control.Comonad
> import Control.Comonad.Cofree
> import Control.Monad.State hiding (sequence)
> import Data.Foldable (Foldable, fold)
> import Data.Maybe (fromMaybe)
> import Data.Monoid
> import Data.Traversable (Traversable, sequence)
> import qualified Data.Map as M

Our little language is an extended lambda calculus with integer and
string literals. The interesting thing here is that the AST doesn't
specify `(AST a)` for recursion, only `a` - this gives us more
flexibility; we can have a node that isn't recursive or even a special
type of recursion (e.g. we'll eventually get annotated recursion).

> data AST a = ALambda String a
>            | AApply a a
>            | ANumber Int
>            | AString String
>            | AIdent String

But for now we want a normal recursive AST. If we use a fixed-point we
can get a normal one using `Mu AST`.

> newtype Mu f = Mu (f (Mu f))

Now it's surprisingly easy to create an unattributed AST. Our example
will use the following lambda expression: (λx.x)2

> example :: Mu AST
> example = Mu $ AApply (Mu . ALambda "x" . Mu $ AIdent "x") (Mu $ ANumber 2)

> num = Mu . ANumber
> var = Mu . AIdent
> lam = (Mu .) . ALambda
> app = (Mu .) . AApply

Later on we'll need to be able to traverse the AST and store it in a
map.

> deriving instance Show a => Show (AST a)
> deriving instance Functor AST
> deriving instance Foldable AST
> deriving instance Traversable AST
> deriving instance Eq a => Eq (AST a)
> deriving instance Ord a => Ord (AST a)

We're going to add types to our AST. `TVar` is a parametric type. For
example, the type `α → α` is represented as `TLambda (TVar 0) (TVar
0)`.

> data Type = TLambda Type Type
>           | TVar Int
>           | TNumber
>           | TString
>
> deriving instance Show Type

Bottom-up inference works by generating constraints required to
calculate the type of each node and propagating them up to the top
level, then solving them.

Our example will only use an equality constraint, asserting that two
types can unify. This should be easy to extend, allowing things like
let-polymorphism or type-classes.

> data Constraint = EqualityConstraint Type Type
>
> deriving instance Show Constraint

Each step of the inference algorithm will give us a set of constraints
and possibly a map of assumptions from identifier to type. For example
we could get a set of `{α ≡ TNumber, β ≡ TString}` with assumptions of
`{hello = {α}}`. We can combine the results of multiple steps using a
monoid instance.

> data TypeResult = TypeResult {
>       constraints :: [Constraint],
>       assumptions :: M.Map String [Type]
>     }
>
> deriving instance Show TypeResult
>
> instance Monoid TypeResult where
>     mempty = TypeResult {
>                constraints = mempty,
>                assumptions = mempty
>              }
>     mappend a b = TypeResult {
>                              constraints = constraints a `mappend` constraints b,
>                              assumptions = assumptions a `mappend` assumptions b
>                            }

We need to keep track of some state during the inference process. We
need the next fresh variable ID and a memoisation map for the result
of inference per AST node.

> data TypeState t m = TypeState {
>       varId :: Int,
>       memo :: M.Map t m
>     }

Each node will return a type along with its constraints and
assumptions. Since we'll be using that a lot inside of the State
Monad, we'll define an alias.

> type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

We have a function to retrieve the next fresh type variable and then
update the ID.

> freshVarId :: State (TypeState t m) Type
> freshVarId = do
>   v <- gets varId
>   modify $ \s -> s { varId = succ v }
>   return $ TVar v

We'll always want to memoise the result of each step so we define a
memoiser that takes the type-inferencing function and stores the
results in the memo map.

> memoizedTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
> memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
>     memoize = do
>       r <- f c
>       modify $ \s -> s { memo = M.insert c r $ memo s }
>       return r

We need to convert our example AST from `Mu` into `Cofree`. Cofree
takes a parameterised type and makes it recursive with each step
having an attribute. Our initial attribution will be unit (i.e. we'll
initially use Cofree just for the recursive comonadic structure).

> cofreeMu :: Functor f => Mu f -> Cofree f ()
> cofreeMu (Mu f) = () :< fmap cofreeMu f

The real annotation function will take a unit annotated Cofree AST
then do a comonadic extend so that each node is annotated with its
type and state. Fairly easy.

But we'll get a Cofree where each attribute is a State operation. We
can sequence to get a combined State of a Cofree AST. Then we can run
the State to get just a Cofree AST with our attributes!

> attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
> attribute c =
>     let initial = TypeState { memo = M.empty, varId = 0 }
>     in evalState (sequence $ extend (memoizedTC generateConstraints) c) initial

Let's take a look at the comonadic operation which generates a type
along with its constraints and assumptions.

> generateConstraints :: Cofree AST () -> TypeCheck (Cofree AST ())

Literals are trivial. They don't need any constraints or
assumptions. We immediately know their type.

> generateConstraints (() :< ANumber _) = return (TNumber, mempty)
> generateConstraints (() :< AString _) = return (TString, mempty)

Using an identifier just creates a fresh type variable and puts it in
the assumption map.

> generateConstraints (() :< AIdent s) = do
>   var <- freshVarId
>   return (var, TypeResult {
>                    constraints = [],
>                    assumptions = M.singleton s [var]
>                  })

A memoised recursive call to the lambda's body is used for the
lambda's return type and for propagating constraints. Lambdas take the
name of their bound variable out of the body's assumption map and turn
it into a constraint for the input of the returned lambda type.

> generateConstraints (() :< ALambda s b) = do
>   var <- freshVarId
>   br <- memoizedTC generateConstraints b
>   let cs = maybe [] (map $ EqualityConstraint var) (M.lookup s . assumptions $ snd br)
>       as = M.delete s . assumptions $ snd br
>   return (TLambda var (fst br), TypeResult {
>                         constraints = constraints (snd br) `mappend` cs,
>                         assumptions = as
>                       })

Lambda application generates constraints for the lambda and the
argument. It then generates a fresh type variable to use as the return
type and; a constraint that the lambda can take the argument and
returns the type variable.

> generateConstraints (() :< AApply a b) = do
>   var <- freshVarId
>   ar <- memoizedTC generateConstraints a
>   br <- memoizedTC generateConstraints b
>   return (var, snd ar `mappend` snd br `mappend` TypeResult {
>                    constraints = [EqualityConstraint (fst ar) $ TLambda (fst br) var],
>                    assumptions = mempty
>                  })

To be able to get a type for the AST, we'll need to solve all of the
constraints. Solving equality constraints is easy, we just try to
unify them which will give a substitution map of type variable ID to
type. We need to put a constraint through that substitution map before
trying to solve it, so that we know we have the latest information
about its type variables.

> solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
> solveConstraints =
>     foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
>           where solve maybeSubs (EqualityConstraint a b) = do
>                   subs <- maybeSubs
>                   mostGeneralUnifier (substitute subs a) (substitute subs b)

So given two types, we need to be able to get a map of substitutions if
the types unify.

> mostGeneralUnifier :: Type -> Type -> Maybe (M.Map Int Type)

If one side is a type variable, then we map that type variable ID to
the type on the other side.

> mostGeneralUnifier (TVar i) b = Just $ M.singleton i b
> mostGeneralUnifier a (TVar i) = Just $ M.singleton i a

When both sides are obviously the same, they can unify with just an
empty substitution map.

> mostGeneralUnifier TNumber TNumber = Just M.empty
> mostGeneralUnifier TString TString = Just M.empty

Lambdas must unify their bound variables and then their bodies. They
must also substitute type variables as soon as they have information
about them.

> mostGeneralUnifier (TLambda a b) (TLambda c d) = do
>     s1 <- mostGeneralUnifier a c
>     liftM2 mappend (mostGeneralUnifier (substitute s1 b) (substitute s1 d)) $ Just s1

If none of the above cases apply then the types can't be the same and
therefore don't unify.

> mostGeneralUnifier _ _ = Nothing

The type substitution using a substitution map is very simple. Type
variables don't get substituted if they don't exist in the substitution
map.

> substitute :: M.Map Int Type -> Type -> Type
> substitute subs v@(TVar i) = -- maybe v (substitute subs) $ M.lookup i subs
>   case M.lookup i subs of
>     Just (TVar j) -> if i == j then v else substitute subs (TVar j)
>     Just t -> substitute subs t
>     Nothing -> v
> substitute subs (TLambda a b) = TLambda (substitute subs a) (substitute subs b)
> substitute _ t = t

Now we can put it all together. We attribute the tree to get a type
and its constraints. We then solve those constraints to get a
substitution map. Finally, we can map over each AST node, discarding
the constraints and applying the substitution map to get a final type.

> typeTree :: Cofree AST () -> Maybe (Cofree AST Type)
> typeTree c =
>     let result = attribute c
>         (r :< _) = result
>         maybeSubs = solveConstraints . constraints $ snd r
>     in fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

Now we can go back to the `cofreeMu` example we wrote above and print:

1. The AST
2. The AST attributed with constraints, assumptions and unsolved types
3. The AST with solved types

main :: IO ()
main = do
  print $ cofreeMu example
  print . attribute $ cofreeMu example
  print . typeTree $ cofreeMu example

The last is the most interesting:

    Just
      (TNumber :< AApply
        (TLambda TNumber TNumber :< ALambda "x"
          (TNumber :< AIdent "x"))
        (TNumber :< ANumber 2))

Awesome. Does exactly what we want. It seems like the type system
allows easy extension. We could even add extra comonadic operations
for different phases of the compiler, like annotating nodes with
type-class dictionaries.

But I can think of two problems with this comonadic approach:

1. We have to explicitly sequence comonadic phases. It'd be better if
we could annotate the AST with a semigroup and then append different
phases in parallel but then we'd probably lose type-safety when trying
to retrieve an attribute.

2. The memoisation is annoying and seems to reflect the way we're
traversing using a comonad. But then, doing it without Cofree seems
even more annoying.

Anyway, time to translate the
[above](https://github.com/puffnfresh/fantasy-states)
[to](https://github.com/puffnfresh/fantasy-cofrees)
[JavaScript](https://github.com/puffnfresh/fantasy-land)...
