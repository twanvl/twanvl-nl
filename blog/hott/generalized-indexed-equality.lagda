title: Generalized indexed equality
tags: hott, ttie
date: 2016-01-02

> -- LANGUAGE: agda
> -- LEXER: agda-fun |Iv|iv|refl|cast
> -- LEXER: sup |\^[a-zA-Z0-9=>-]+
> -- LEXER: sub |_[a-zA-Z0-9=>-]+

\begin{code}
{-# OPTIONS --rewriting #-}
module _ where

open import Util.Equality as Meta using (_∎) renaming (_≡_ to _⟹_; refl to □; _≡⟨_⟩_ to _⟹⟨_⟩_; _≡⟨_⟩⁻¹_ to _⟸⟨_⟩_)
open import Function
\end{code}

This post builds on <a href="">the previous post</a> on indexed equality.
The idea there is that equality types are 'indexed' by the homotopy interval.
This interval type consists of two values, but these values are considered equal, that is to say, there is a path betwee them.
So the interval is equivalent to the truncated booleans, which also has two values and a path between them.

Now here is a silly idea: instead of using the truncated booleans as index type in our equality, why not allow for any truncated type?

Let's start with truncation
\begin{code}
postulate Truncate : ∀ {a} (A : Set a) → Set a -- HIDE a
postulate [_] : ∀ {a A} → A → Truncate {a} A -- HIDE a
\end{code}

truncation forms a monad,

\begin{code}
postulate bind : ∀ {a b A B} → (A → Truncate {b} B) → Truncate {a} A → Truncate {b} B -- HIDE a|b
postulate bind-[] : ∀ {a b A B} f x → bind {a} {b} {A} {B} f [ x ] ⟹ f x
postulate bind-id : ∀ {a A} x → bind {a} {a} {A} [_] x ⟹ x -- HIDE a|b
postulate bind-const : ∀ {a b A B} x y → bind {a} {b} {A} {B} (\_ → x) y ⟹ x
{-# REWRITE bind-[] bind-id bind-const #-}
\end{code}

define map in terms of bind
\begin{code}
map : ∀ {a b A B} → (A → B) → Truncate {a} A → Truncate {b} B -- HIDE a|b
map f = bind ([_] ∘ f)
\end{code}

Now the indexed equality type is
\begin{code}
data Eq {a} {I : Set} (A : Truncate I → Set a) : ((i : I) → A [ i ]) → Set a where
  refl : (f : ∀ i → A i) → Eq A (f ∘ [_])
\end{code}

And the eliminator
\begin{code}
infixl 6 _^_
postulate _^_  : ∀ {a I A x} → Eq {a} {I} A x → (i : Truncate I) → A i
postulate ^-[]   : ∀ {a I A x} (xy : Eq {a} {I} A x) i → xy ^ [ i ] ⟹ x i
postulate ^-refl : ∀ {a I A} x i → refl {a} {I} {A} x ^ i ⟹ x i
{-# REWRITE ^-[] ^-refl #-}
\end{code}

Compare this to the 

Let's define a type with two elements
\begin{code}
data Two : Set where
  b₀ b₁ : Two

bcase : ∀ {a} {A : Two → Set a} → A b₀ → A b₁ → (b : Two) → A b -- HIDE a
bcase x y b₀ = x
bcase x y b₁ = y
\end{code}

Using this two-element type for the index gives us back the interval indexed equality from previous posts.
\begin{code}
Interval : Set
Interval = Truncate Two

i₀ i₁ : Interval
i₀ = [ b₀ ]
i₁ = [ b₁ ]

icase : ∀ {I : Set} → Truncate I → Truncate I → Interval → Truncate I
icase x₀ x₁ = bind (bcase x₀ x₁)

IEq : ∀ {a} (A : Interval → Set a) → A i₀ → A i₁ → Set a -- HIDE a
IEq A x₀ x₁ = Eq A (bcase x₀ x₁)

_≡_ : ∀ {a} {A : Set a} → A → A → Set a
_≡_ {A = A} x y = IEq (\_ → A) x y
\end{code}

-- Transport

Transport is inherently two-sided, since you transport a value from some source type to a target type. So that doesn't really change

\begin{code}
postulate tr : ∀ {a} (A : Interval → Set a) → A i₀ → A i₁
\end{code}

-- Univalance

What would univalance look like for this generalized equality type?
Instead of an equivalance between two types, we need an equavalance between any number of types.

What would be the generalization of equivalances? Instead of relating two types, we need to relate any number of types.

Instead of an equivalance between two types, we need an equavalance between any number of types.

I propose the following:
\begin{code}
record Equiv {a} {I : Set} (A : I → Set a) : Set a where
  field cast : ∀ i j → A i → A j
  field cast-id : ∀ i x → cast i i x ≡ x
  field cast-trans : ∀ i j k x → cast j k (cast i j x) ≡ cast i k x
open Equiv
\end{code}
This is enough to get a substitutive relation that is reflexive and transitive

\begin{code}
postulate univalence : ∀ {a I A} → Equiv {a} {I} A → Eq (\_ → Set a) A
\end{code}

One not so nice thing here is that we

\begin{code}
postulate tr-univalence : ∀ {a I A} (e : Equiv {a} {I} A) i j → ¬ (i ⟹ j)
                        → tr (\k → univalence e ^ icase [ i ] [ j ] k) ⟹ cast e i j
\end{code}

This leads to a problem, however, because


----------------

This post builds on <a href="">the previous post</a> on indexed equality.
I should warn that todays ideas are a bit silly, and maybe not so useful.
In the last post I defined the equality type as

> data Eq (A : Iv → Type) : A 0 → A 1 → Type where
>   refl : (x : (i : Iv) → A i) → Eq A (x 0) (x 1)

Note that the equalities/paths have two end points, corresponding to the two end points of the interval. But this is not the only possible choice.

The interval is actually a special case of a truncation:

> data Truncate (A : Type) : Type where
>   [_]   : (x : A) → Truncate A
>   trunc : (x : A) → (y : A) → Eq _ [ x ] [ y ]
> 
> Iv = Truncate Bool

It is also possible to write a generalization of the indexed equality type. Instead of paths between two points, these are generalized paths between a certain number of points:

> data GEq {I : Type} (A : Truncate I → Type) : ((i : I) → A [ i ]) → Type where
>   grefl : (f : (i : Truncate I) -> A x) → GEq A (\i → f [ i ])
> 
> Eq A x y = GEx A (\i → if i then x else y)

What @GEq f@ is in effect saying is that @f@ does not essentially depend on its argument.

Furthermore, the @trunc@ higher constructor could also use a generalized equality instead of a normal one,

> data Truncate (A : Type) : Type where
>   inject : (x : A) -> Truncate A
>   trunc  : GEq_i (Truncate A) inject

then just as @01 = refl id@, we also have the generalization @trunc = grefl id@.
//In the remainder of this post I will write the eliminator of this trun

Now the first question you might ask is what the relationship is between generalized equalities with different indices.

GEq {Bool} (\i -> 




Now there are a couple of questions:
 * What is 

> gcast : GEq Type A → A i → A j
> gcast : {I : Type} (A : Truncate I → Type) → A i → A j

@gcast_i=0→1 A x@

> gcast {

The eliminator for Truncate is

> tr : {A} {B : Truncate A → Type} (inj : (x : A) → B [ x ]) (GEq_i (B [ i ]) inj)


-------------

The relation between the booleans and the interval is truncation, i.e. the interval are the truncated booleans.
If you like generalizing, then you can consider a more general equality type, with not the truncated booleans, but any truncated index type A, as far as I know this doesn't buy you anything compared to what I have written in this post. But you can argue that it looks prettier

> -- homotopy truncation
> Trunc : Type → Type
> box   : ∀ {A} → A → Trunc A
> -- Eq A f asserts that all values of f are equal
> Eq    : ∀ {A} (B : Trunc A → Type) → (f : (x : A) → B (box x))
> refl  : ∀ {A} {B} (f : (x : Trunc A) → B x) → Eq B (f ∘ box)
> -- truncated values are equal
> (f : 
> unbox : {A} {P : Trunc A → Type} → → (x : Trunc A) → P x

The normal two sided (indexed) equality is a special case where @A = Bool@,
> Interval = Trunc Bool
> UsualEq A x y = Eq {Bool} A (\i → case i of {false -> x; true -> y})



-----------------

Generalize univalance:
  univalence : (A : K → Set) → (f : ∀ i j → A i → A j) → (∀ i → f i ≡ id) → (∀ i j k → f j k ∘ f i j ≡ f i k) → GEq Set A

Nice :)

-}
-------------
Generalized equality looks like:

> data GEq {I : Type} (A : Truncate I → Type) : ((i : I) → A [ i ]) → Type where
>   grefl : (f : (i : Truncate I) -> A i) → GEq A (\i → f [ i ])
> 

The only magic that you need is in truncation

> data Truncate (A : Type) : Type where
>   inject : (x : A) -> Truncate A

> trunc : GEq (\i -> Truncate A) inject
> 

Truncation forms a monad,

> bind : Truncate A -> (A -> Truncate B) -> Truncate B
> map


> map-pure : map f (pure x) = pure (f x)
> map-const : map (const x) y = pure x

We want to write functions on truncated types:

> map-trunc : (A -> B) -> Truncate A -> Truncate B
> map-trunc f (inject x) = inject (f x)

> (f : Truncate A -> X) -> (g : Truncate B -> X) ->  -> (Truncate (Either A B) -> X)

> (f : Truncate (Maybe A) -> X) -> (g : Truncate (Maybe B) -> X) -> f nothing == g nothing -> (Truncate (Maybe (Either A B)) -> X)
