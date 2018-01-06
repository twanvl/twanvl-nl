title: cong from refl in univalent OTT
subtitle: ... and hence subst from refl
date: 2013-07-04 18:00 CEST
source link: This post is literate Agda, get the source
language: agda
tags: agda, hott
literate style: latex

\begin{code}
-- HIDDEN
-- this is the same stuff as last time
module cong-from-refl where

open import Level
open import Function
open import Data.Unit
open import Data.Bool
open import Data.Empty
open import Data.Product
open import Relation.Binary.PropositionalEquality as Meta using (_≡_)

postulate Path : ∀ {a} → (A : Set a) → A → A → Set a
postulate refl : ∀ {a} → (A : Set a) → (x : A) → Path A x x

postulate Path-⊤ : Path ⊤ tt tt ≡ ⊤
postulate Path-Bool00 : Path Bool false false ≡ ⊤
postulate Path-Bool01 : Path Bool false true ≡ ⊥
postulate Path-Bool10 : Path Bool true false ≡ ⊥
postulate Path-Bool11 : Path Bool true true ≡ ⊤

Π : ∀ {a b} (A : Set a) (B : A → Set b) → Set (a ⊔ b)
Π A B = (x : A) → B x

Iso : ∀ {a} → (A B : Set a) → Set a
Iso {a} A B
  = Σ (A → B) \fw →
    Σ (B → A) \bw →
    (∀ x → Path A (bw (fw x)) x) ×
    (∀ y → Path B (fw (bw y)) y)

id-Iso : ∀ {a} → (A : Set a) → Iso A A
id-Iso A = (id , id , refl A , refl A)

postulate Path-Type : ∀ {a} (A B : Set a)
                    → Path (Set a) A B ≡ Lift {a} {suc a} (Iso A B)
\end{code}

This is a follow up on <a href="blog/agda/subst-from-cong">last week's post</a>.
There I showed that in a univalent Observational Type Theory, you can derive @subst@ from @cong@.
Now I am going to go one step further.

Suppose we change the definition of paths for functions from
> Path (A → B) f g ≡ ∀ x → f x ≡ g x
to
> Path (A → B) f g ≡ ∀ {x y} → x ≡ y → f x ≡ g y

Then for a function @f@, @refl f@ is actually the same thing as @cong f@!.
So that's one less primitive to worry about. In fact the only two path related primitives that remain are @Path@ and @refl@. The rest is just in the computation rules.

Here are the changes in the agda code compared to last week:
\begin{code}
postulate Path-→ : ∀ {a b} {A : Set a} {B : Set b} (f g : A → B)
                 → Path (A → B) f g
                 ≡ ((x y : A) → Path A x y → Path B (f x) (g y))

-- cong = refl
cong : ∀ {a b} {A : Set a} {B : Set b}
     → (f : A → B) → ∀ {x y} → Path A x y → Path B (f x) (f y)
cong f x=y = Meta.subst id (Path-→ f f) (refl _ f) _ _ x=y

-- subst is the same as last time
subst : ∀ {a b} {A : Set a} (B : A → Set b)
      → {x y : A} → (Path A x y) → B x → B y
subst B {x} {y} p with Meta.subst id (Path-Type (B x) (B y)) (cong B p)
... | lift (fw , bw , _ , _) = fw

-- and paths for dependent functions
postulate Path-Π : ∀ {a b} {A : Set a} {B : A → Set b} (f g : Π A B)
                 → Path (Π A B) f g
                 ≡ ((x y : A) → (pa : Path A x y)
                              → Path (B y) (subst B pa (f x)) (g y))
\end{code}

Of course this doesn't really change anything, since defining @refl@ for function types is no easier than defining @cong@.

-- Representation --

You might also notice that for all types @A@ (except @Set@), the structure of @Path A@ is essentially the same as that of @A@. In fact, for a (non-indexed) data type
> data Foo : Set where
>   foo₀ : Foo
>   foo₁ : A → Foo
>   foo₂ : Foo → Foo → Foo
you can mechanically derive its path type to be
> data Path Foo : Foo → Foo → Set where
>   refl-foo₀  : Path (foo₀ x) (foo₀ x)
>   cong₁-foo₁ : ∀ {x x'} → Path A x x' → Path Foo (foo₁ x) (foo₁ x')
>   cong₂-foo₂ : ∀ {x x' y y'} → Path Foo x x' → Path Foo y y'
>                               → Path Foo (foo₂ x y) (foo₂ x' y')

In theory this allows for a nice implementation trick: we can take the representation of @x@ and @refl x@ to be the same. So for example @5 : Path Int 5 5@ is a path that asserts that 5 = 5, and it is the only such path.

Originally I thought that an implementation would have to pass @cong f@ along with every parameter @f@ of a function type (which would suck). But in this way we don't have to, since @f@ and @cong f@ are the same function.

This also corresponds nicely to the idea that extra path constructors can be added in Higher Inductive Types. But I am not quite sure yet how that works out.

-- Food for thought --

* What is @refl _→_@?
* What is @refl refl@? Does this even make sense?
* For the representation of @x : A@ and @refl x@ to be the same, @A@ and @Path A x x@ also need to have the same representation. That seems works for functions and inductive types, but what about @Set@?
* Is @Path@ an applicative functor in some sense? With @refl@ as return and @cong@ as ap?

