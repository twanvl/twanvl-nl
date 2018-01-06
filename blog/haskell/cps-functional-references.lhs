title: CPS based functional references
date: 2009-07-20
tags: haskell, lens
sourcelink: This post is literate Haskell, click here to download the source code.

I have recently come up with a new way of representing functional references.

As you might recall, functional references (also called lenses) are like a pointer into a field of some data structure.
The value of this field can be extracted and modified.
For example:

]> GHCi> get fstF (123,"hey")
]> 123
]> GHCi> set fstF 456 (123,"hey")
]> (456,"hey")
]> GHCi> modify fstF (*2) (123,"hey")
]> (246,"hey")

where @fstF@ is a functional reference to the first element of a pair.
It has the type @RefF (a,b) a@, i.e. in a 'record' of type @(a,b)@ it points to an @a@.

Previous representations relied on a record that contained the @get@ and @set@ or the @get@ an @modify@ functions.
But there is a much nicer looking representation possible using Functors.


<br>First of all we will need a language extension and some modules:

> {-# LANGUAGE Rank2Types #-}
> import Control.Applicative
> import Control.Monad.Identity

Now the representation for functional references I came up with is:

> type RefF a b = forall f. Functor f => (b -> f b) -> (a -> f a)

This type looks a lot like a continuation passing style function, which would be simply @(b -> r) -> (a -> r)@, but where the result is @f a@ instead of any @r@.
With different functors you get different behaviors. With the constant functor we can @get@ the field pointed to:

> get :: RefF a b -> a -> b
> get r = getConst . r Const

While the identity functor allows a function us to @modify@ the field:

> modify :: RefF a b -> (b -> b) -> a -> a
> modify r m = runIdentity . r (Identity . m)
> 
> set :: RefF a b -> b -> a -> a
> set r b = modify r (const b)

As an example of an 'instance', here is the @fstF@ function I used in the introduction:

> fstF :: RefF (a,b) a
> fstF a_to_fa (a,b) = (\a' -> (a',b)) <$> a_to_fa a

If we had <a href="http://hackage.haskell.org/trac/ghc/ticket/3377">tuple sections</a> it could be written as simply
]> fstF x (a,b) = (,b) <$> x a



<br>To get access to inner fields, functional references can be composed. So @compose fstF fstF@ points to the first element inner inside the first outer element of a nested pair.
One of the things that I like about the cps/functor based representation is that composition is quite beautiful and symmetric:

> compose :: RefF b c -> RefF a b -> RefF a c
> compose r s = s . r
> 
> idF :: RefF a a
> idF = id

Let me conclude with the pair operator, called @(***)@ in Control.Arrow.
Unfortunately this operator is not as easy to define.

> pair :: RefF a c -> RefF b d -> RefF (a,b) (c,d)
> pair r s cd_to_fcd (a,b) = some_ugly_code

In fact, the only way I know of implementing pair is by moving back and forth to a get/set representation

>  where some_ugly_code =
>          let fcd = cd_to_fcd (get r a, get s b)      -- :: f (c,d)
>              cd_to_ab (c,d) = (set r c a, set s d b) -- :: (c,d) -> (a,b)
>          in fmap cd_to_ab fcd                        -- :: f (a,b)

The problem is that we need to split one function of type @(c,d) -> f (c,d)@ into two, @c -> f c@ and @d -> f d@, because that is what the left and right arguments expect.
Then later, we would need to do the reverse and combine two of these functions again.

Does anyone have a better suggestion for implementing @pair@?
