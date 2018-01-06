title: Substitution from congruence in univalent OTT
subtitle: It is sufficient to assume `cong` in order to get all properties of identity types
date: 2013-06-22 16:51 CEST
source link: This post is literate Agda, get the source
language: agda
tags: agda, hott
literate style: latex

In this post I will show that in an univalence style observational type theory, it is enough to take congruence as a primitive, rather than the more complicated substitution or J axioms. This post is literate Agda, so here are some boring import declarations

\begin{code}
module subst-from-cong where

open import Level
open import Function
open import Data.Unit
open import Data.Bool
open import Data.Empty
open import Data.Product
\end{code}

I will be using the standard propositional equality as a meta equality,

\begin{code}
open import Relation.Binary.PropositionalEquality as Meta using (_≡_)
\end{code}

while postulating a path type (equality type) and its computation rules for me to prove things about,

\begin{code}
postulate Path : ∀ {a} → (A : Set a) → A → A → Set a
postulate refl : ∀ {a} → (A : Set a) → (x : A) → Path A x x
\end{code}

The idea of Observational Type Theory (OTT) is that @Path@ is actually defined by case analysis on the structure of the argument type. For the finite types this is simple, there is a path if and only if the values are the same,

\begin{code}
postulate Path-⊤ : Path ⊤ tt tt ≡ ⊤

postulate Path-Bool00 : Path Bool false false ≡ ⊤
postulate Path-Bool01 : Path Bool false true ≡ ⊥
postulate Path-Bool10 : Path Bool true false ≡ ⊥
postulate Path-Bool11 : Path Bool true true ≡ ⊤
\end{code}

A path for functions is a function to paths, which also means that we have functional extensionality.

\begin{code}
Π : ∀ {a b} (A : Set a) (B : A → Set b) → Set (a ⊔ b)
Π A B = (x : A) → B x

postulate Path-Π : ∀ {a b} {A : Set a} {B : A → Set b} (f g : Π A B)
                 → Path (Π A B) f g ≡ ((x : A) → Path (B x) (f x) (g x))
\end{code}

In their <a href="http://www.cs.nott.ac.uk/~txa/publ/obseqnow.pdf">original OTT paper</a>, Alternkirch et.al. defined equality for types also by structure matching. I.e. Π types are equal to Π types with equal arguments, Σ types are equal to Σ types, etc.
But this is incompatible with the univalence axiom from Homotopy Type Theory. That axiom states that equivalent or isomorphic types are equal. So, what happens if we take isomorphism as our definition of equality between types?

\begin{code}
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

Now suppose that we have a congruence, i.e. that all functions preserve paths. So from a path between @x@ and @y@, we can construct a path between @f x@ and @f y@ for any function @f@. 

\begin{code}
-- we have congruence for non-dependent functions
postulate cong : ∀ {a b} {A : Set a} {B : Set b}
               → (f : A → B) → ∀ {x y} → Path A x y → Path B (f x) (f y)
\end{code}

Then this is enough to define substitution, since the paths for a type @B x@ are isomorphisms, and we can apply these in the forward direction

\begin{code}
subst : ∀ {a b} {A : Set a} (B : A → Set b) {x y : A} → (Path A x y) → B x → B y
subst B {x} {y} p with Meta.subst id (Path-Type (B x) (B y)) (cong B p)
... | lift (fw , bw , _ , _) = fw
\end{code}

With substitution we can now finally define what paths are for dependent Σ types.
A path between pairs is a pair of paths,

\begin{code}
postulate Path-Σ : ∀ {a b} {A : Set a} {B : A → Set b} (x y : Σ A B)
                 → Path (Σ A B) x y
                 ≡ Σ (Path A (proj₁ x) (proj₁ y))
                     (\pa → Path (B (proj₁ y)) (subst B pa (proj₂ x)) (proj₂ y))
\end{code}

Substitution is not the most general eliminator for paths.
It is not enough to prove properties about paths. For that we need the general induction principle for paths, often called J

\begin{code}
J : ∀ {a b} {A : Set a} {x : A} → (B : (y : A) → Path A x y → Set b)
  → {y : A} → (p : Path A x y) → B x (refl A x) → B y p
\end{code}

Unfortunately, I was unable to prove J from just congruence. For that I needed an additional lemma,

\begin{code}
postulate subst-refl : ∀ {a} {A : Set a} {x y : A} → (p : Path A x y)
                     → p ≡ subst (Path A x) p (refl A x)
\end{code}

Since @Path A@ is inductively defined, I believe that @subst-refl@ should be provable by case analysis on @A@, but I have not yet done so. We can now implement J by using @subst@ with a dependent pair.
Note that here I have to manually apply the comptuation rules for @Path (Σ _ _)@ and use the @subst-refl@ lemma.

\begin{code}
J {A = A} {x = x} B {y} p
  = subst (uncurry B)
      (Meta.subst id (Meta.sym $ Path-Σ (x , refl A x) (y , p)) $
      (p , Meta.subst (\q → Path (Path A x y) q p) (subst-refl p)
           (refl (Path A x y) p)))
\end{code}

-- Does it compute --

An important question to ask is whether this style of OTT is actually implementable.
We can certainly implement the definitions, but would they allow us to compute?

The type @Path A@ certainly reduces, by definition. Similarly, it is not hard to implemenent @refl@.
The hard part is defining what @cong@ means for various functions, and then proving @subst-refl@.
Somewhere in there we should put the fact that paths are transitive and symmetric, since we have not used that property so far. For what I have done up till now I could equally well have taken @Iso A B = A → B@.

Here are the implementations of @refl@,

\begin{code}
_≡[_]≡_ : ∀ {a} {A B : Set a} → A → A ≡ B → B → Set a
a ≡[ p ]≡ b = Meta.subst id p a ≡ b

postulate
  refl-⊤     : refl ⊤ tt ≡[ Path-⊤ ]≡ tt
  refl-Bool0 : refl Bool false ≡[ Path-Bool00 ]≡ tt
  refl-Bool1 : refl Bool true  ≡[ Path-Bool11 ]≡ tt
  refl-Π     : ∀ {a b} {A : Set a} {B : A → Set b} (f : Π A B)
             → refl (Π A B) f ≡[ Path-Π f f ]≡ (\x → refl (B x) (f x))
  refl-Type  : ∀ {a} (A : Set a)
             → refl (Set a) A ≡[ Path-Type A A ]≡ lift (id-Iso A)
\end{code}

For @refl (Σ _ _)@ we need yet another lemma, which is a bit a dual to @subst-refl₁@, allowing @refl@ in the second argument instead of the third.

\begin{code}
postulate
  subst-refl₁ : ∀ {a b} {A : Set a} {B : A → Set b} {x : A} {y : B x}
              → y ≡ subst B (refl A x) y

  refl-Σ : ∀ {a b} {A : Set a} {B : A → Set b} (x : Σ A B)
         → refl (Σ A B) x ≡[ Path-Σ x x ]≡
          (refl A (proj₁ x) ,
           Meta.subst (\x1 → Path (B (proj₁ x)) x1 (proj₂ x))
                      (subst-refl₁ {B = B} {y = proj₂ x})
                      (refl (B (proj₁ x)) (proj₂ x)))
\end{code}

And here is a start of the implementation of @cong@,

\begin{code}
postulate
  cong-const : ∀ {a b} {A : Set a} {B : Set b} {x x'} {y} {p : Path A x x'}
             → cong (\x → y) p ≡ refl B y
  cong-id    : ∀ {a} {A : Set a} {x x'} {p : Path A x x'}
             → cong (\x → x) p ≡ p
  cong-∘     : ∀ {a b c} {A : Set a} {x x'} {p : Path A x x'}
                 {B : Set b} {C : Set c} {f : B → C} {g : A → B}
             → cong (\x → f (g x)) p ≡ cong f (cong g p)
  -- etc.
\end{code}

At some point I think you will also need a dependent @cong@.

But this is enough postulating for one day.
