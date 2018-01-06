title: Isomorphism lenses
subtitle: My new favorite implementation of lenses. It gives you the laws for free
date: 2011-05-22 16:23:12Z
tag: haskell, lens
sourcelink: This post is literate Haskell, click here to download the source code.

In the past I have <a href="/tag/lens">blogged about functional references</a>.
From now on I will conform to most of the rest of the world, and call these things lenses.

Giving a presentation on these objects has forced me to think about them some more.
As a result of this thinking I have a new favorite representation, at least from a theory point of view:

<blockquote style='font-style:italic'>A lens from type @a@ to @b@ is a bijection between @a@ and a pair of @b@ and some residual @r@.</blockquote>

In pseudo-code we would write that as
]> -- BLOCK: haskell-pseudo-code
]> type Lens a b = exists r. a <-> (b,r)

Except that Haskell has no @exists@ keyword, so you have to use a newtype wrapper:

> -- HIDDEN
> {-# LANGUAGE ExistentialQuantification #-}
> import Control.Category
> import Control.Arrow
> import Prelude hiding (id,(.))
 
> -- Isomorphisms/bijections between type @a@ and @b@
> data Iso a b = Iso { fw :: a -> b, bw :: b -> a }
> 
> -- Lenses with a data wrapper, in practice you might want to unpack the Iso type
> data Lens a b = forall r. Lens (Iso a (b,r))


So, why do I like this representation so much?

--Intuition--

I believe this representation captures the intuition of what a lens does extremely well:
You have some record type @a@, and you want to take out and a field of (a smaller) type @b@. When you do that you are left with some residual, which you can think of as @a-b@ (or should that be @a/b@?).

I imagine this graphically as<br>
<img src="image/lens/isolens1.png" style="margin-left:2em;margin-top:.5em;">,<br>
where we have a square record @a@, containing a smaller circular field of type @b@.

Implementing the usual @get@, @modify@ and @set@ functions is now very easy, by going back and forth through the lens.

> get :: Lens a b -> a -> b
> get (Lens l) = fst . fw l
> 
> modify :: Lens a b -> (b -> b) -> (a -> a)
> modify (Lens l) f = bw l . first f . fw l
> 
> set :: Lens a b -> b -> a -> a
> set l b a = modify l (const b) a

The nice thing about the existential quantification is that the residual type @r@ can be anything you like.
In some cases it is obvious what it could be, such as the case of tuples:

> myFst :: Lens (a,b) a
> myFst = Lens (Iso id id) -- r = b

but we could also pick any other representation,

> myCrazyFst :: Lens (a,String) a
> myCrazyFst = Lens (Iso fw bw) -- r = strings starting with "Banana"
>    where fw (a,b) = (a, "Banana" ++ b)
>          bw (a,'B':'a':'n':'a':'n':'a':b) = (a,b)

For this to be an actual isomorphism we have to restrict the residual to only strings that start with @"Banana"@. That is not something we can actually enforce in Haskell, but then again, we don't check that a lens is an isomorphism at all. 

Besides the simple intuition and the freedom in declaring them, there is another reason for liking these lenses.

--Laws--

There are two (or one, depending on how you count) obvious laws you want isomorphisms to satisfy:
]> -- BLOCK: haskell-equational-reasoning
]> fw i . bw i = bw i . fw i = id

On the other hand, there are several less obvious laws for lenses:
]> -- BLOCK: haskell-equational-reasoning
]> set l (get l a) a = a
]> get l (set l b a) a = b
]> set l c (set l b a) = set l c a

And now comes the magic: with isomorphism lenses all of these laws follow from the simple laws of isomorphisms.
Here are the quick and dirty proofs.
One:
]> -- BLOCK: haskell-equational-reasoning
]>   set l (get l a) a
]> = {- expanding definitions of get and set -}
]>   (bw l . first (const ((fst . fw l) a)) . fw l) a
]> = {- let x = fw l a, rewrite -}
]>   bw l (first (const (fst x)) x) where x = fw l a
]> = {- (first (const (fst x)) x) = x -}
]>   bw l x where x = fw l a
]> = {- fill in x and rewrite -}
]>   (bw l . fw l) a
]> = {- isomorphism law -}
]>   a
Two:
]> -- BLOCK: haskell-equational-reasoning
]>   get l (set l b a) a
]> = {- expanding definitions of get and set, rewrite to use composition -}
]>   fst . fw l . bw l . first (const b) . fw l $ a
]> = {- isomorphism law -}
]>   fst . first (const b) . fw l $ a
]> = {- expanding fst, first and const -}
]>   (\(x,y) -> x) . (\(x,y) -> (b,y)) . fw l $ a
]> = {- composing the two lambda terms -}
]>   const b . fw l $ a
]> = {- definition of const -}
]>   b
Three:
]> -- BLOCK: haskell-equational-reasoning
]>   set l c (set l b a)
]> = {- expanding definition of set, rewrite to use composition -}
]>   bw l . first (const c) . fw l . bw l . first (const b) . fw l $ a
]> = {- isomorphism law -}
]>   bw l . first (const c) . first (const b) . fw l $ a
]> = {- first f . first g = first (f . g) -}
]>   bw l . first (const c . const b) . fw l $ a
]> = {- const c . const b = const c -}
]>   bw l . first (const c) . fw l $ a
]> = {- definition of set -}
]>   set l c a

So there you have it. A simple representation of lenses that gives a nice intuition of what these things actually do. And as an added bonus, the laws for lenses follow directly from the definition.
Finally I should say that this representation is not my idea, it has been around in the literature for quite some time.

