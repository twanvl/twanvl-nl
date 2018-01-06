title: Categories over pairs of types
subtitle: Pipes with upstream results are a Haskell category
date: 2012-07-26 21:33 CEST
tags: haskell, pipes

Today <a href="http://unknownparallel.wordpress.com/2012/07/26/pipes-and-conduits-part-2-upstream-results/">Dan Burton remarked that</a> Pipe is a category-like thing, and to express it we would need "type bundling". I myself <a href="blog/haskell/results-of-upstream-pipes">said something similar</a> a while ago. More formally, rather than a category where the objects are Haskell types, we have a category where the objects are pairs of types.

It turns out that with a bunch of recent Ghc extensions we ''can'' actually write this in Haskell.

> {-# LANGUAGE PolyKinds, DataKinds, KindSignatures #-}
> {-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

> -- IGNORE: boring stuff.
> import Prelude hiding (id,(.))
> import Control.Monad

For the purposes of this blogpost I'll use a dummy type for @Pipe@, there are plenty of other blog posts that give an actually functional one. The important thing to note is that in the type of @(>+>)@, there are two types that are composed over, input/output @io@ and upstream/downstream result @ur@.

> -- Ceci n'est pas une pipe
> data Pipe i o u m r = Pipe { runPipe :: Either i u -> m (Either o r) }
> 
> (>+>) :: Monad m
>       => Pipe io__1 io__2 ur__1 m ur__2
>       -> Pipe io__2 io__3 ur__2 m ur__3
>       -> Pipe io__1 io__3 ur__1 m ur__3
> (>+>) (Pipe f) (Pipe g) = Pipe (f >=> g)
> 
> idP :: Monad m => Pipe i i r m r
> idP = Pipe return

With the @PolyKinds@ extension we can make a variant of @Category@ that works for tuples of types as well as for normal types. This class looks exactly the same as the normal one:

> class Category cat where
>     id :: cat a a
>     (.) :: cat b c -> cat a b -> cat a c

But because of @PolyKinds@ it magically becomes more general. You can see this by comparing their kinds

]> λ> :kind Category
]> Category :: (AnyK -> AnyK -> *) -> Constraint
]> λ> :kind Control.Category.Category
]> Control.Category.Category :: (* -> * -> *) -> Constraint


With @DataKinds@ it becomes possible to have tuples of types, which are written as @'(Type1,Type2)@.
Unfortunately we can not (yet?) pattern match on these directly in data declarations. So we need type families to unwrap them:

> type family Fst (xy :: (*,*)) :: *
> type family Snd (xy :: (*,*)) :: *
> type instance Fst '(x,y) = x
> type instance Snd '(x,y) = y

Note that the kind signatures are necessary. Without them Ghc will give errors like

] Couldn't match kind `BOX' against `*'

With these type functions in hand we can write

> newtype WrapPipe m iu or = WrapPipe
>      { unWrapPipe :: Pipe (Fst iu) (Fst or) (Snd iu) m (Snd or) }
> 
> instance Monad m => Category (WrapPipe m) where
>     id = WrapPipe idP
>     x . y = WrapPipe (unWrapPipe y >+> unWrapPipe x)

And that's it. We now have a category whose objects are not Haskell types, but rather pairs of Haskell types. In Ghc's terms, an instance of @Category (*,*)@ instead of @Category *@. The kind parameter is why we need @MultiParamTypeClasses@.

With this same trick we can also define @Category@ instances for product categories and lens families. Or going the other way, you can wrap @Monoids@ as a @Category@ over objects of kind @()@. You could even go one step further and have a category for lists of functions of different types.

There is a big downside, however. And that is that the type inference engine is not able to see past the type families. You need to give an explicit type annotation on the wrapped pipe. Compare

> -- HIDDEN: this doesn't actually work from Ghci
> type MyPipe = Pipe Int Int String IO String
> type MyWPipe = WrapPipe IO '(Int,String) '(Int,String)
> test = ('(','a')

]> λ> type MyWPipe = WrapPipe IO '(Int,String) '(Int,String)
]> λ> runPipe (unWrapPipe (id . id :: MyWPipe)) $ Right "done"
]> Right "done"

with

]> λ> type MyPipe = Pipe Int Int String IO String
]> λ> runPipe (unWrapPipe (id . id) :: MyPipe) $ Right "done"
]> <interactive>:2:10:
]>    Couldn't match type `Fst or0' with `Int'
]>    blah, blah, blah, etc.

This makes sense, since Ghc doesn't know that there are no other instances of @Fst@ and @Snd@. Ideally we would like to write

]> --BLOCK: haskell-pseudo-code
]> newtype WrapPipe m '(i,u) '(o,r) = WrapPipe { unWrapPipe :: Pipe i o u m r }



//One thing that I have not yet managed is to get a kind for restricted types, e.g. the kind of types that are an instance of @Ord@. For that you probably need dependent types like @Σ(a : Type). (Ord a)@.

