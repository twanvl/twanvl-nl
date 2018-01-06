title: Traversing syntax trees
subtitle: A convenient traversal function on which to build substitution and more
tags: haskell
date: 2017-08-23 2:26 CEST
source-link: This post is literate Haskell, click here to download the source code.
discussion: <a href="https://www.reddit.com/r/haskell/comments/6vgxvp/traversing_syntax_trees/">Reddit</a>

> -- HIDDEN
> {-# LANGUAGE ScopedTypeVariables #-}
> import Control.Applicative
> import Control.Monad.Identity
> import Data.Coerce
> import Data.Monoid
> import Data.Char
> import qualified Data.Set as Set
> import Data.Set (Set)

When working with syntax trees (such as in <a href="https://www.twanvl.nl/blog/hott/indexed-equality-implementation">a type theory interpreter</a>) you often want to apply some operation to all subtrees of a node, or to all nodes of a certain type. Of course you can do this easily by writing a recursive function. But then you would need to have a case for every constructor, and there can be many constructors.

Instead of writing a big recursive function for each operation, it is often easier to use a traversal function. Which is what this post is about. In particular, I will describe my favorite way to handle such traversal, in the hope that it is useful to others as well.

As a running example we will use the following data type, which represents expressions in a simple lambda calculus

> -- Lambda calculus with de Bruijn indices
> data Exp
>   = Var !Int
>   | Lam Exp
>   | App Exp Exp
>   | Global String
>   deriving Show
> 
> example__1 :: Exp
> example__1 = Lam $ Var 0 -- The identity function
> 
> example__2 :: Exp
> example__2 = Lam $ Lam $ Var 1 -- The const function
> 
> example__3 :: Exp
> example__3 = Lam $ Lam $ Lam $ App (Var 2) (App (Var 1) (Var 0)) -- Function composition

Now, what do I mean by a traversal function?
The base library comes with the @Traversable@ class, but that doesn't quite fit our purposes, because that class is designed for containers that can contain any type a. But expressions can only contain other sub-expressions.
Instead we need a monomorphic variant of @traverse@ for our expression type:

]> -- LANGUAGE: haskell
]> traverseExp :: Applicative f => (Exp -> f Exp) -> (Exp -> f Exp)

The idea is that @traverseExp@ applies a given function to all direct children of an expression.

The <a href="http://hackage.haskell.org/package/uniplate">uniplate</a> package defines a similar function, <a href="http://hackage.haskell.org/package/uniplate-1.6.12/docs/Data-Generics-Uniplate-Operations.html#v:descendM">@descendM@</a>. But it has two problems: 1) @descendM@ has a @Monad@ constraint instead of @Applicative@, and 2) the class actually requires you to implement a @uniplate@ method, which is more annoying to do.

The ever intimidating <a href="http://hackage.haskell.org/package/lens">lens</a> package has a closer match in <a href="http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html">@plate@</a>. But aside from the terrible name, that function also lacks a way to keep track of bound variables.

For a language with binders, like the lambda calculus, many operations need to know which variables are bound.
In particular, when working with de Bruijn indices, it is necessary to keep track of the number of bound variables. To do that we define

> type Depth = Int
> -- Traverse over immediate children, with depth
> traverseExpD :: Applicative f => (Depth -> Exp -> f Exp) -> (Depth -> Exp -> f Exp)
> traverseExpD _ _ (Var i)    = pure (Var i)
> traverseExpD f d (Lam x)    = Lam <$> f (d+1) x
> traverseExpD f d (App x y)  = App <$> f d x <*> f d y
> traverseExpD _ _ (Global x) = pure (Global x)

Once we have written this function, other traversals can be defined in terms of @traverseExpD@

> -- Traverse over immediate children
> traverseExp :: Applicative f => (Exp -> f Exp) -> (Exp -> f Exp)
> traverseExp f = traverseExpD (const f) 0

And map and fold are just traversals with a specific applicative functor, @Identity@ and @Const a@ respectively. Recent versions of GHC are smart enough to know that it is safe to @coerce@ from a traversal function to a mapping or folding one.

> -- Map over immediate children, with depth
> mapExpD :: (Depth -> Exp -> Exp) -> (Depth -> Exp -> Exp)
> mapExpD = coerce (traverseExpD :: (Depth -> Exp -> Identity Exp) -> (Depth -> Exp -> Identity Exp))
> 
> -- Map over immediate children
> mapExp :: (Exp -> Exp) -> (Exp -> Exp)
> mapExp = coerce (traverseExp :: (Exp -> Identity Exp) -> (Exp -> Identity Exp))
> 
> -- Fold over immediate children, with depth
> foldExpD :: forall a. Monoid a => (Depth -> Exp -> a) -> (Depth -> Exp -> a)
> foldExpD = coerce (traverseExpD :: (Depth -> Exp -> Const a Exp) -> (Depth -> Exp -> Const a Exp))
> 
> -- Fold over immediate children
> foldExp :: forall a. Monoid a => (Exp -> a) -> (Exp -> a)
> foldExp = coerce (traverseExp :: (Exp -> Const a Exp) -> (Exp -> Const a Exp))


After doing all this work, it is easy to answer questions like "how often is a variable used?"

> varCount :: Depth -> Exp -> Sum Int
> varCount i (Var j)
>   | i == j   = Sum 1
> varCount i x = foldExpD varCount i x

or "what is the set of all free variables?"

> freeVars :: Depth -> Exp -> Set Int
> freeVars d (Var i)
>   | i < d     = Set.empty             -- bound variable
>   | otherwise = Set.singleton (i - d) -- free variable
> freeVars d x = foldExpD freeVars d x

Or to perform (silly) operations like changing all globals to lower case

> lowerCase :: Exp -> Exp
> lowerCase (Global x) = Global (map toLower x)
> lowerCase x = mapExp lowerCase x

These functions follows a common pattern of specifying how a particular constructor, in this case @Var@ or @Global@, is handled, while for all other constructors traversing over the child expressions.


As another example, consider substitution, a very important operation on syntax trees.
In its most general form, we can combine substitution with raising expressions to a larger context (also called weakening).
And we should also consider leaving the innermost, bound, variables alone. This means that there are three possibilities for what to do with a variable.

> substRaiseByAt :: [Exp] -> Int -> Depth -> Exp -> Exp
> substRaiseByAt ss r d (Var i)
>   | i < d           = Var i -- A bound variable, leave it alone
>   | i-d < length ss = raiseBy d (ss !! (i-d)) -- substitution
>   | otherwise       = Var (i - length ss + r) -- free variable, raising
> substRaiseByAt ss r d x = mapExpD (substRaiseByAt ss r) d x

Similarly to @varCount@, we use @mapExpD@ to handle all constructors besides variables.
Plain substitution and raising are just special cases.

> -- Substitute the first few free variables, weaken the rest
> substRaiseBy :: [Exp] -> Int -> Exp -> Exp
> substRaiseBy ss r = substRaiseByAt ss r 0
> 
> raiseBy :: Int -> Exp -> Exp
> raiseBy r = substRaiseBy [] r
> 
> subst :: [Exp] -> Exp -> Exp
> subst ss = substRaiseBy ss 0

]> λ> raiseBy 2 (App (Var 1) (Var 2))
]> App (Var 3) (Var 4)
]> 
]> λ> subst [Global "x"] (App (Var 0) (Lam (Var 0)))
]> App (Global "x") (Lam (Var 0))
]> 
]> λ> substRaiseBy [App (Global "x") (Var 0)] 2 $ App (Lam (App (Var 1) (Var 0))) (Var 2)
]> App (Lam (App (App (Global "x") (Var 1)) (Var 0))) (Var 3)


As a slight generalization, it can also make sense to put @traverseExpD@ into a type class. That way we can traverse over the subexpressions inside other data types. For instance, if the language uses a separate data type for case alternatives, we might write

]> -- LANGUAGE: haskell
]> data Exp
]>   = ...
]>   | Case [Alt]
]> 
]> data Alt
]>   = Alt Pat Exp
]> 
]> class TraverseExp a where
]>   traverseExpD :: Applicative f => (Depth -> Exp -> f Exp) -> (Depth -> a -> f a)
]> 
]> instance TraverseExp a => TraverseExp [a] where
]>   traverseExpD f d = traverse (traverseExpD f d)
]> 
]> instance TraverseExp Exp where
]>   traverseExpD f d ...
]>   traverseExpD f d (Case xs) = Case <$> traverseExpD f d xs
]> 
]> instance TraverseExp Alt where
]>   traverseExpD f d (Alt x y) = Alt x <$> traverseExpD f (d + varsBoundByPat x) y

Another variation is to track other things besides the number of bound variables. For example we might track the names and types of bound variables for better error messages. And with a type class it is possible to track different aspects of bindings as needed,

]> -- LANGUAGE: haskell
]> class Env env where
]>   extend :: VarBinding -> env -> env
]> 
]> instance Env Depth where
]>   extend _ = (+1)
]> 
]> instance Env [VarBinding] where
]>   extend = (:)
]> 
]> instance Env () where
]>   extend _ _ = ()
]> 
]> traverseExpEnv :: Applicative f => (env -> Exp -> f Exp) -> (env -> Exp -> f Exp)
]> traverseExpEnv f env (Lam name x) = Lam <$> f (extend name env) x
]> traverseExpEnv f env ...

Overall, I have found that after writing @traverseExpD@ once, I rarely have to look at all constructors again. I can just handle the default cases by traversing the children.

A nice thing about this pattern is that it is very efficient. The @traverseExpD@ function is not recursive, which means that the compiler can inline it. So after optimization, a function like @lowerCase@ or @varCount@ is exactly what you would have written by hand.

