title: Type theory with indexed equality - the theory
subtitle: The theory behind TTIE
tags: hott, ttie
language: agda
source link: This post is literate Agda
date: 2018-01-04 22:18 CET

In <a href="https://www.twanvl.nl/blog/hott/indexed-equality-implementation">a previous post</a> I introduced the TTIE language, along with a <a href="https://github.com/twanvl/ttie">type checker and interpreter</a>.
My motivation for writing that (aside from it being fun!) was to explore the type system.
At the time I started this project, formalizing this system as a shallow embedding in Agda was not easy. But with the addition of a rewriting mechanism, it has become much easier to use Agda without going insane from having to put substitutions everywhere. So, in this post I will formalize the TTIE type system.

This post is literate Agda, and uses my own utility library. The utility library mainly defines automatic rewrite rules like @trans x (sym x) ≡ refl@, which make life a bit more pleasant. All these rewrites use the standard library propositional equality @≡@, which I will call ''meta equality''.
. All these rewrites use the standard library propositional equality, which I will denote as @⟹@ and call ''meta equality''.

> -- LEXER: agda-ctor |\b(refl|□|■|done|eq|cons₁|cons₂|i0|i1|i₀|i₁|point|loop|base|surf)(?=$|[ )\]}])|∷|\[\]|■
> -- LEXER: agda-fun |\b(cast|jay|inot|gets|(ext|cong|sym|trans|map|icase|tr\b|univalence|refl-|sides|refls|eqs-)[a-z0-9₀-₉'′-]*)\b
> -- LEXER: conid |\bI\b

\begin{code}
{-# OPTIONS --rewriting #-}
module _ where

open import Util.Equality as Meta using (_∎) renaming (_≡_ to _⟹_; refl to □; _≡⟨_⟩_ to _⟹⟨_⟩_; _≡⟨_⟩⁻¹_ to _⟸⟨_⟩_)
open import Data.Product
open import Data.Sum
open import Data.Nat using (ℕ; zero; suc)
open import Data.Vec
open import Function
open import Level renaming (zero to lzero; suc to lsuc)
\end{code}

First we postulate the existence of the interval. I will abbreviate the interval type as @I@.

\begin{code}
postulate I : Set
postulate i₀ : I
postulate i₁ : I
\end{code}

The canonical eliminator for the interval needs equalities, to show that @i₀@ and @i₁@ are mapped to equal values. But we haven't defined those yet. However, there is one eliminator that we can define, namely into @I@, since values in @I@ are always equal.

\begin{code}
postulate icase : I → I → I → I
postulate icase-i₀ : ∀ a b → icase a b i₀ ⟹ a
postulate icase-i₁ : ∀ a b → icase a b i₁ ⟹ b
{-# REWRITE icase-i₀ icase-i₁ #-}
\end{code}

And with this @icase@ construct, we can define conjunction, disjunction, and negation

\begin{code}
_&&_ : I → I → I
i && j = icase i₀ j i

_||_ : I → I → I
i || j = icase j i₁ i

inot : I → I
inot = icase i₁ i₀
\end{code}

We can define some extra computation rules based on the principle that when evaluating @icase a b c@, if we use the @a@ branch then @c = i₀@, and similarly for @b@.
\begin{code}
postulate icase-same : ∀ (a b c : I → I) d → a i₀ ⟹ c i₀ → b i₁ ⟹ c i₁
                     → icase (a d) (b d) d ⟹ c d

icase-const : ∀ a b → icase a a b ⟹ a
icase-id    : ∀ a   → icase i₀ i₁ a ⟹ a
icase-i₀-x  : ∀ b   → icase i₀ b b ⟹ b
icase-i₁-x  : ∀ b   → icase i₁ b b ⟹ i₁
icase-x-i₀  : ∀ a   → icase a i₀ a ⟹ i₀
icase-x-i₁  : ∀ a   → icase a i₁ a ⟹ a

-- COLLAPSED: Show implementation
icase-const a b = icase-same (const a) (const a) (const a) b □ □
icase-id    a   = icase-same (const i₀) (const i₁) id a □ □
icase-i₀-x  b   = icase-same (const i₀) id id b □ □
icase-i₁-x  b   = icase-same (const i₁) id (const i₁) b □ □
icase-x-i₀  a   = icase-same id (const i₀) (const i₀) a □ □
icase-x-i₁  a   = icase-same id (const i₁) id a □ □

{-# REWRITE icase-const #-}
{-# REWRITE icase-id #-}
{-# REWRITE icase-i₀-x #-}
{-# REWRITE icase-i₁-x #-}
{-# REWRITE icase-x-i₀ #-}
{-# REWRITE icase-x-i₁ #-}
\end{code}

\begin{code}
-- HIDDEN
-- And some more
icase-icase : ∀ a b c d e → icase a b (icase c d e) ⟹ icase (icase a b c) (icase a b d) e
icase-icase a b c d e = Meta.sym $ icase-same (\e → icase a b c) (\e → icase a b d) (icase a b ∘ icase c d) e □ □
{-# REWRITE icase-icase #-}
&&-&&-|| : ∀ a b → icase i₀ (icase i₀ (icase b i₁ a) b) a ⟹ a && b -- (a && b) && (a || b) ⟹ a && b
&&-&&-|| a b = Meta.sym $ icase-same (const i₀) (const b) (\a → icase i₀ (icase b i₁ a) (icase i₀ b a)) a □ □
&&-||-|| : ∀ a b → icase (icase b i₁ a) (icase (icase b i₁ a) i₁ b) a ⟹ a || b -- (a && b) || (a || b) ⟹ a || b
&&-||-|| a b = Meta.sym $ icase-same (const b) (const i₁) (\a → icase (icase b i₁ a) i₁ (icase i₀ b a)) a □ □
&&-|| : ∀ a b → icase i₀ (icase b i₁ a) a ⟹ a -- a && (a || b) ⟹ a
&&-|| a b = icase-same (const i₀) (icase b i₁) id a □ □
{-# REWRITE &&-&&-|| #-}
{-# REWRITE &&-||-|| #-}
{-# REWRITE &&-|| #-}
\end{code}




//------------------------------------------------------------------------------
-- The equality type
//------------------------------------------------------------------------------

We can now define the indexed equality type

\begin{code}
data Eq {a} (A : I → Set a) : A i₀ → A i₁ → Set a where
  refl : ∀ (x : (i : I) → A i) → Eq A (x i₀) (x i₁)
\end{code}

For convenience we write the non-indexed object level equality as

\begin{code}
_≡_ : ∀ {a} {A : Set a} → A → A → Set a
_≡_ {A = A} x y = Eq (\_ → A) x y
\end{code}

And now that we have equalities, we can write down the the general dependent eliminator for the interval, 

\begin{code}
postulate _^_ : ∀ {a A x y} → Eq {a} A x y → (i : I) → A i
postulate ^-i₀   : ∀ {a A x y} x≡y → _^_ {a} {A} {x} {y} x≡y i₀ ⟹ x
postulate ^-i₁   : ∀ {a A x y} x≡y → _^_ {a} {A} {x} {y} x≡y i₁ ⟹ y
postulate ^-refl : ∀ {a A} x → _^_ {a} {A} {x i₀} {x i₁} (refl x) ⟹ x
{-# REWRITE ^-i₀ ^-i₁ ^-refl #-}
infixl 6 _^_
\end{code}

At the same time, the @_^_@ operator also functions as an eliminator for @Eq@, projecting out the argument to @refl@. This also means that we have the following eta contraction rule

\begin{code}
refl-eta : ∀ {a A x y} (x≡y : Eq {a} A x y) → refl (\i → x≡y ^ i) ⟹ x≡y -- HIDE a
refl-eta (refl x) = □
{-# REWRITE refl-eta #-}
\end{code}

These definitions are enough to state some object level theorems, such as function extensionality

\begin{code}
ext′ : ∀ {a} {A B : Set a} {f g : A → B} → (∀ x → f x ≡ g x) → f ≡ g -- HIDE a
ext′ f≡g = refl \i → \x → f≡g x ^ i
\end{code}

congruence,

\begin{code}
cong′ : ∀ {a b} {A : Set a} {B : Set b} (f : A → B) {x y} → x ≡ y → f x ≡ f y -- HIDE a|b
cong′ f x≡y = refl \i → f (x≡y ^ i)
\end{code}

and symmetry of @≡@,

\begin{code}
sym′ : ∀ {a} {A : Set a} {x y : A} → x ≡ y → y ≡ x -- HIDE a
sym′ x≡y = refl \i → x≡y ^ inot i
\end{code}

We can also define dependent versions of all of the above, which are the same, only with more general types. I'll leave these as an exercise for the reader.

\begin{code}
-- COLLAPSED: spoiler
sym : ∀ {a} {A : I → Set a} {x y} → Eq A x y → Eq (A ∘ inot) y x
sym x≡y = refl \i → x≡y ^ inot i
\end{code}

//------------------------------------------------------------------------------
-- Transport
//------------------------------------------------------------------------------

In general, to make full use of equalities, you would use substitution, also called ''transport''.
I will formalize this as

\begin{code}
postulate tr : ∀ {a} (A : I → Set a) → A i₀ → A i₁ -- HIDE a
\end{code}

Where @tr@ stands for transport, since we transport a value of type @A i₀@ along @A@, to a value of type @A i₁@.
This should be possible, because there is a path between @i₀@ and @i₁@, that is, they are indistinguishable, and because functions are continuous. So @A@ is a continuous path between @A i₀@ and @A i₁@.
In a previous blog post I have used a more general @cast@ primitive, which can be defined in terms of @tr@,

\begin{code}
cast : ∀ {a} (A : I → Set a) → (j₀ j₁ : I) → A j₀ → A j₁ -- HIDE a
cast A j₀ j₁ = tr (\i → A (icase j₀ j₁ i))
\end{code}

And now we can define things like the usual substitution
\begin{code}
subst : ∀ {a b} {A : I → Set a} (B : {i : I} → A i → Set b) {x} {y} → Eq A x y → B x → B y -- HIDE a|b
subst B xy = tr (\i → B (xy ^ i))
\end{code}

and the J axiom

\begin{code}
jay : ∀ {A : Set} {x : A} (B : {y : A} → x ≡ y → Set) → {y : A} → (x≡y : x ≡ y)
    → B (refl (\_ → x)) → B x≡y
jay B xy = tr (\i → B {xy ^ i} (refl \j → xy ^ (j && i)))
\end{code}

Yay, jay!

//------------------------------------------------------------------------------
-- Evaluating transport
//------------------------------------------------------------------------------

To be useful as a theory of computation, all primitives in our theory should reduce.
In particular, we need to know how to evaluate @tr@, at least when it is applied to arguments without free variables.
We do this by pattern matching on the first argument of @tr@, and defining transport for each type constructor.

The simplest case is if the type being transported along doesn't depend on the index at all
\begin{code}
postulate tr-const : ∀ {a} {A : Set a} {x} → tr (\_ → A) x ⟹ x -- HIDE a
{-# REWRITE tr-const #-}
\end{code}

Much more interesting is the case when the type is a function type.
To cast function types, we first transport the argument 'back', apply the function, and then transport the result forward. First look at the non-dependent case, i.e. going from @A i₀ → B i₀@ to @A i₁ → B i₁@:

\begin{code}
postulate tr-arrow : ∀ {a b} {A : I → Set a} {B : I → Set b} {f} -- HIDE a|b
                   → tr (\i → A i → B i) f
                   ⟹ (\x → tr B (f (cast A i₁ i₀ x)))
\end{code}

The dependent case is a bit more complicated, since the type of the result depends on the transported argument.
The result of the function has type @B i₀ (cast A i₁ i₀ x)@, and we have to transport this to @B i₁ x@. So as we go from @i₀@ to @i₁@, we want to "undo" the @cast@ operation.
We can do this by changing both @i₀@'s to @i₁@'s, to get a value of the type @B i₁ (cast A i₁ i₁ x)@. Because @cast A i₁ i₁ x ⟹ x@ by @icase-const@ and @tr-const@, this is equivalent to @B i₁ x@.

\begin{code}
postulate tr-pi : ∀ {a b} {A : I → Set a} {B : (i : I) → (A i) → Set b} {f} -- HIDE a|b
                → tr (\i → (x : A i) → B i x) f
                ⟹ (\x → tr (\i → B i (cast A i₁ i x)) (f (cast A i₁ i₀ x)))
\end{code}

Besides function/pi types, there are also product/sigma types.
The idea here is similar: transport both parts of the pair independently. Again, the type of the second part can depend on the transported first part,

\begin{code}
postulate tr-sigma : ∀ {a b} {A : I → Set a} {B : (i : I) → A i → Set b} {x y} -- HIDE a|b
                      → tr (\i → Σ (A i) (B i)) (x , y)
                      ⟹ (tr A x , tr (\i → B i (cast A i₀ i x)) y)
\end{code}

Finally, let's look at sum types, for which we use simple recursion,

\begin{code}
postulate tr-sum₁ : ∀ {a b} {A : I → Set a} {B : I → Set b} {x} -- HIDE a|b
                  → tr (\i → A i ⊎ B i) (inj₁ x) ⟹ inj₁ (tr A x)
postulate tr-sum₂ : ∀ {a b} {A : I → Set a} {B : I → Set b} {x} -- HIDE a|b
                  → tr (\i → A i ⊎ B i) (inj₂ x) ⟹ inj₂ (tr B x)
\end{code}



//------------------------------------------------------------------------------
-- Transport for equality types
//------------------------------------------------------------------------------

The final type constructors in our language are equality types, and this is where things get more hairy.
The idea is that a type like @Eq A x y@ behaves like @A@ in many respects. Its values will just be wrapped in a @refl@ constructor.

Consider the case of equalities over (dependent) function types.
The evaluation rule could look like

\begin{code}
postulate tr-eq-pi
           : ∀ {a b} {A : I → I → Set a} -- HIDE a|b
               {B : ∀ i j → A i j → Set b} -- HIDE a|b
               {u : ∀ i → (x : A i i₀) → B i i₀ x}
               {v : ∀ i → (x : A i i₁) → B i i₁ x}
               {f₀ : Eq (\j → (x : A i₀ j) → B i₀ j x) (u i₀) (v i₀)}
           → tr (\i → Eq (\j → (x : A i j) → B i j x) (u i) (v i)) f₀
           ⟹ refl \j → \x →
             let x' = \i' j' → tr (\i → A (icase i₁ i' i) (icase j j' i)) x in
             (tr (\i → Eq (\j' → B i j' (x' i j')) (u i (x' i i₀)) (v i (x' i i₁)))
                 (refl \j' → (f₀ ^ j') (x' i₀ j'))) ^ j
\end{code}

Of course the @A@ in @Eq A x y@ could again be an equality type, and we would have to repeat the construction.
To do this systematically, I start by collecting all the 'sides' of the equality type recursively.
For example the sides of @Eq (\i → Eq (\j → _) x y) u v)@ are @eq (\i → eq (\j → done) x y) u v@,

\begin{code}
mutual
  data Sides {a} : ∀ n (A : Vec I n → Set a) → Set (lsuc a) where
    done : ∀ {A} → Sides zero A
    eq   : ∀ {n A}
         → (sides : (i : I) → Sides n (\is → A (i ∷ is)))
         → Eqs (sides i₀)
         → Eqs (sides i₁)
         → Sides (suc n) A

  Eqs : ∀ {a n A} → Sides {a} n A → Set a
  Eqs {A = A} done = A []
  Eqs {A = A} (eq sides x y) = Eq (\i → Eqs (sides i)) x y
\end{code}

Since @I → A@ are the continuous functions out of the 1-dimensional interval,
you can think of a @Vec I n → A@ as a continuous function out of the n-dimensional hypercube. So in geometric terms, we can draw such a function as assigning a value to all elements of the hypercube.
Similarly, you can think of @Sides {n = n}@ as a function out of the n-dimensional hypercube with the central cell removed, and @Eqs@ as filling in that central cell.
<table style="text-align:center">
<tr>
  <td style="text-align:center">@Eqs 0@
  <td style="text-align:center">@Sides 1@
  <td style="text-align:center">@Eqs 1@
  <td style="text-align:center">@Vec I 1 → A@
  <td style="text-align:center">@Sides 2@
  <td style="text-align:center">@Eqs 2@
  <td style="text-align:center">@Vec I 2 → A@
<tr>
<td style="text-align:center">
  <svg width="20" height="20">
    <g transform="translate(10,10)">
      <circle cx="0" cy="0" r="4" fill="black"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="20" height="120">
    <g transform="translate(10,60)">
      <circle cx="0" cy="-50" r="4" fill="black"/>
      <circle cx="0" cy="50" r="4" fill="black"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="20" height="120">
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="4" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L3,4 L0,8 L9,4 z"/>
    </marker>
    <g transform="translate(10,60)">
      <circle cx="0" cy="-50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <circle cx="0" cy="50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <line x1="0" y1="-44" x2="0" y2="44" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="20" height="120">
    <g transform="translate(10,60)">
      <circle cx="0" cy="-50" r="4" fill="black"/>
      <circle cx="0" cy="50" r="4" fill="black"/>
      <line x1="0" y1="-44" x2="0" y2="44" stroke="black" stroke-width="1"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="120" height="120">
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="4" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L3,4 L0,8 L9,4 z"/>
    </marker>
    <g transform="translate(60,60)">
      <circle cx="-50" cy="-50" r="4" fill="black"/>
      <circle cx="-50" cy="50" r="4" fill="black"/>
      <circle cx="50" cy="-50" r="4" fill="black"/>
      <circle cx="50" cy="50" r="4" fill="black"/>
      <line x1="-44" y1="-50" x2="44" y2="-50" stroke="black" stroke-width="1"/>
      <line x1="-44" y1="50" x2="44" y2="50" stroke="black" stroke-width="1"/>
      <line x1="-50" y1="-38" x2="-50" y2="38" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
      <line x1="50" y1="-38" x2="50" y2="38" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="120" height="120">
    <marker id="grayArrow" markerWidth="10" markerHeight="10" refX="9" refY="4" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L3,4 L0,8 L9,4 z" fill="none" stroke="#888" stroke-dasharray="2,2"/>
    </marker>
    <g transform="translate(60,60)">
      <circle cx="-50" cy="-50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <circle cx="-50" cy="50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <circle cx="50" cy="-50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <circle cx="50" cy="50" r="4" fill="none" stroke="#888" stroke-dasharray="2,2"/>
      <line x1="-44" y1="-50" x2="44" y2="-50" stroke="#888" stroke-dasharray="2,2" stroke-width="1"/>
      <line x1="-44" y1="50" x2="44" y2="50" stroke="#888" stroke-dasharray="2,2" stroke-width="1"/>
      <line x1="-50" y1="-38" x2="-50" y2="38" stroke="#888" stroke-dasharray="2,2" stroke-width="1" marker-end="url(#grayArrow)"/>
      <line x1="50" y1="-38" x2="50" y2="38" stroke="#888" stroke-dasharray="2,2" stroke-width="1" marker-end="url(#grayArrow)"/>
      <rect x="-44" y="-40" width="88" height="80" fill="#8cf"/>
      <line x1="-3" y1="-20" x2="-3" y2="20" stroke="black" stroke-width="1.5"/>
      <line x1="3" y1="-20" x2="3" y2="20" stroke="black" stroke-width="1.5"/>
      <path d="M-10,15 L0,24 L10,15" stroke="black" stroke-width="1.5" fill="none"/>
    </g>
  </svg>
<td style="text-align:center">
  <svg width="120" height="120">
    <g transform="translate(60,60)">
      <path d="M-50,-50 L50,-50 L50,50 L-50,50 z" stroke="black" stroke-width="1" fill="#8cf"/>
      <circle cx="-50" cy="-50" r="4" fill="black"/>
      <circle cx="-50" cy="50" r="4" fill="black"/>
      <circle cx="50" cy="-50" r="4" fill="black"/>
      <circle cx="50" cy="50" r="4" fill="black"/>
    </g>
  </svg>
</table>

I will spare you the details, see the source code of this post if you are interested.
Suffice to say, that if we generalize @_^_@, @icase@, etc. from @I@ to @Vec I n@ and from @Eq@ to @Eqs@, then we can generalize @tr-eq-pi@ to arbitrarily deep @Eqs@.

\begin{code}
-- HIDDEN
-- Generalize icase to a vector of I's
icases : ∀ {n} → Vec I n → Vec I n → I → Vec I n
icases [] [] i = []
icases (x ∷ xs) (y ∷ ys) i = icase x y i ∷ icases xs ys i

icases-i₀ : ∀ {n} is js → icases {n} is js i₀ ⟹ is
icases-i₀ [] [] = □
icases-i₀ (i ∷ is) (j ∷ js) = Meta.cong (_∷_ _) (icases-i₀ is js)
{-# REWRITE icases-i₀ #-}

icases-i₁ : ∀ {n} is js → icases {n} is js i₁ ⟹ js
icases-i₁ [] [] = □
icases-i₁ (i ∷ is) (j ∷ js) = Meta.cong (_∷_ _) (icases-i₁ is js)
{-# REWRITE icases-i₁ #-}

icases-const : ∀ {n} is j → icases {n} is is j ⟹ is
icases-const [] j = □
icases-const (i ∷ is) j = Meta.cong (_∷_ _) (icases-const is j)
{-# REWRITE icases-const #-}

icases-id : ∀ {n} i → icases {n} (replicate i₀) (replicate i₁) i ⟹ replicate i
icases-id {n = zero}  i = □
icases-id {n = suc _} i = Meta.cong (_∷_ _) (icases-id i)
{-# REWRITE icases-id #-}
\end{code}

\begin{code}
-- HIDDEN
_∘∷_ : ∀ {a b A n} {B : Vec {a} A (suc n) → Set b} → (f : ∀ xs → B xs) → (x : A) → (xs : Vec A n) → B (x ∷ xs) -- HIDE a|b
f ∘∷ x = \xs → f (x ∷ xs)
\end{code}

\begin{code}
-- HIDDEN
 
-- We need meta level extensionality for the following proofs
postulate ext : ∀ {a b} → Meta.Extensionality a b

-- cong-ap f≡f' x = cong (\f → f x) f≡f',
-- but it plays nicer with rewriting, since we can't pattern match on things with lambdas.
-- with cong-ap we can define the rewrite rule cong-ap-ext below
cong-ap : ∀ {a b} {A : Set a} {B : A → Set b} {f f' : (x : A) → B x} → f ⟹ f' → (x : A) → f x ⟹ f' x
cong-ap □ x = □

postulate cong-ap-ext : ∀ {a b A B f f'} f≡f' x → cong-ap (ext {a} {b} {A} {B} {f} {f'} f≡f') x ⟹ f≡f' x
{-# REWRITE cong-ap-ext #-}

-- Dependent congruence for eq and refl
cong-eq : ∀ {a n A sides sides' x₀ y₀ x₁ y₁}
        → (s≡s' : sides ⟹ sides')
        → Meta.subst Eqs (cong-ap s≡s' i₀) x₀ ⟹ y₀
        → Meta.subst Eqs (cong-ap s≡s' i₁) x₁ ⟹ y₁
        → eq {a} {n} {A} sides x₀ x₁ ⟹ eq sides' y₀ y₁
cong-eq □ □ □ = □

cong-refl : ∀ {a n A s s' x x'}
          → (s≡s' : s ⟹ s')
          → (x≡x' : (\i → Meta.subst Eqs (cong-ap s≡s' i) (x i)) ⟹ x')
          → Meta.subst Eqs (cong-eq {a} {n} {A} s≡s' (cong-ap x≡x' i₀) (cong-ap x≡x' i₁)) (refl x)
          ⟹ refl x'
cong-refl □ □ = □

-- Generalization of _^_ to n dimensions
gets : ∀ {a n A} sides → Eqs {a} {n} {A} sides → (is : Vec I n) → A is
gets done       x [] = x
gets (eq s _ _) x (i ∷ is) = gets (s i) (x ^ i) is

mutual
  sides : ∀ {a n A} → (x : ∀ is → A is) → Sides {a} n A
  sides {n = zero}  x = done
  sides {n = suc _} x = eq (\i → sides (x ∘∷ i)) (refls (x ∘∷ i₀)) (refls (x ∘∷ i₁))
  refls : ∀ {a n A} → (x : ∀ is → A is) → Eqs (sides {a} {n} {A} x)
  refls {n = zero}  x = x []
  refls {n = suc _} x = refl \i → refls (x ∘∷ i)

mutual
  map-sides : ∀ {a b n A B} → (f : ∀ {is} → A is → B is) → Sides {a} n A → Sides {b} n B
  map-sides f done = done
  map-sides f (eq s x₀ x₁) = eq (map-sides f ∘ s) (map-eqs f (s i₀) x₀) (map-eqs f (s i₁) x₁)
  map-eqs : ∀ {a b n A B} → (f : ∀ {is} → A is → B is) → ∀ s → Eqs s → Eqs (map-sides {a} {b} {n} {A} {B} f s)
  map-eqs f done       x = f x
  map-eqs f (eq s _ _) x = refl \i → map-eqs f (s i) (x ^ i)

-- dependent version of map-sides
mutual
  dmap-sides : ∀ {a b c n} {A : Vec I n → Set a} {B : Vec I n → Set b}
             → (C : ∀ is → B is → Set c)
             → {f : ∀ {is} → A is → B is}
             → (g : ∀ {is} → (x : A is) → C is (f x))
             → (s : Sides n A)
             → (fx : Eqs (map-sides f s))
             → Sides n (\is → C is (gets (map-sides f s) fx is))
  dmap-sides C g done fx = done
  dmap-sides C g (eq s s₀ s₁) fx =
    eq (\i → dmap-sides (C ∘∷ i) g (s i) (fx ^ i))
       (dmap-eqs (C ∘∷ i₀) g (s i₀) s₀)
       (dmap-eqs (C ∘∷ i₁) g (s i₁) s₁)
  dmap-eqs : ∀ {a b c n} {A : Vec I n → Set a} {B : Vec I n → Set b}
           → (C : ∀ is → B is → Set c)
           → {f : ∀ {is} → A is → B is}
           → (g : ∀ {is} → (x : A is) → C is (f x))
           → (s : Sides n A)
           → (x : Eqs s)
           → Eqs (dmap-sides C g s (map-eqs f s x))
  dmap-eqs C g done x = g x
  dmap-eqs C g (eq s s₀ s₁) x = refl \i → dmap-eqs (C ∘∷ i) g (s i) (x ^ i)
\end{code}

\begin{code}
-- HIDDEN
-- eta expansion doesn't affect sides
mutual
  sides-eta : ∀ {a b n}
                {A : Vec I n → Set a}
                {B : (is : Vec I n) → A is → Set b}
                (s : Sides n (\js → (x : A js) → B js x))
            →   let s-eta = \is (x : A is) → map-sides (\{is'} f' → f' (tr (A ∘ icases is is') x)) s in
                (f : ∀ is x → Eqs (s-eta is x))
            → sides (\is x → gets (s-eta is x) (f is x) is) ⟹ s
  sides-eta done f = □
  sides-eta (eq s x₀ x₁) f = cong-eq (ext \i → sides-eta (s i) _) (eqs-eta (s i₀) x₀) (eqs-eta (s i₁) x₁)

  eqs-eta : ∀ {a b n}
              {A : Vec I n → Set a}
              {B : (is : Vec I n) → A is → Set b}
              (s : Sides n (\js → (x : A js) → B js x))
              (f : Eqs s)
          →   let s-eta = \is (x : A is) → map-sides (\{is'} f' → f' (tr (A ∘ icases is is') x)) s in
              let f-eta = \is (x : A is) → map-eqs   (\{is'} f' → f' (tr (A ∘ icases is is') x)) s f in
              Meta.subst Eqs (sides-eta s f-eta) (refls (\is x → gets (s-eta is x) (f-eta is x) is)) ⟹ f
  eqs-eta done f = □
  eqs-eta {A = A} {B = B} (eq s s₀ s₁) f =
    let s-eta' = \i is x → map-sides (\{is'} f' → f' (tr ((A ∘∷ i) ∘ icases is is') x)) (s i) in
    let f-eta' = \i is x → map-eqs   (\{is'} f' → f' (tr ((A ∘∷ i) ∘ icases is is') x)) (s i) (f ^ i) in
    cong-refl {A = \js → (x : A js) → B js x}
              {x = \i → refls (\is x → gets (s-eta' i is x) (f-eta' i is x) is)}
              (ext \i → sides-eta (s i) (f-eta' i))
              (ext \i → eqs-eta (s i) (f ^ i))
\end{code}

\begin{code}
-- HIDDEN
-- This is the right hand side of tr-eqs-pi
-- compare tr-eq-pi above
eqs-pi' : ∀ {a b n} {A : I → Vec I n → Set a} -- HIDE a|b
            {B : (i : I) → (is : Vec I n) → A i is → Set b} -- HIDE a|b
        → (sides : (i : I) → Sides n (\js → (x : A i js) → B i js x))
        → Eqs (sides i₀)
        → ∀ js → (x : A i₁ js) → B i₁ js x
eqs-pi' {A = A} s f₀ is = \x →
  let x' = \i' is' → tr (\i → A (icase i₁ i' i) (icases is is' i)) x in
  gets (map-sides (\{is'} f → f (x' i₁ is')) (s i₁))
       (tr (\i → Eqs (map-sides (\{is'} f → f (x' i is')) (s i)))
           (map-eqs (\{is'} f → f (x' i₀ is')) (s i₀) f₀)) is

\end{code}

\begin{code}
tr-eqs-pi-rhs : ∀ {a b n} {A : I → Vec I n → Set a} -- HIDE a|b
                {B : (i : I) → (is : Vec I n) → A i is → Set b} -- HIDE a|b
              → (sides : (i : I) → Sides n (\js → (x : A i js) → B i js x))
              → Eqs (sides i₀)
              → Eqs (sides i₁)
\end{code}
\begin{code}
-- HIDDEN
tr-eqs-pi-rhs {A = A} sides f₀ = Meta.subst Eqs (sides-eta (sides i₁) _) (refls (eqs-pi' sides f₀))
\end{code}

\begin{code}
postulate tr-eqs-pi : ∀ {a b n}
                        {A : I → Vec I n → Set a}
                        {B : (i : I) → (is : Vec I n) → A i is → Set b}
                        (sides : (i : I) → Sides n (\js → (x : A i js) → B i js x))
                        (f₀ : Eqs (sides i₀))
                    → tr (Eqs ∘ sides) f₀
                    ⟹ tr-eqs-pi-rhs sides f₀
\end{code}

You can do a similar thing for sigma types, except that the types get even messier there because we need a dependently typed @map@ function for @Eqs@ and @Sides@.

\begin{code}
-- HIDDEN
mutual
  -- eta expansion for pairs
  sides-pair : ∀ {a b n}
                 {A : Vec I n → Set a}
                 {B : (is : Vec I n) → A is → Set b}
                 (s : Sides n (\js → Σ (A js) \x → B js x))
                 (x : Eqs (map-sides proj₁ s))
                 (y : Eqs (dmap-sides B proj₂ s x))
             → sides (\is → gets (map-sides proj₁ s) x is
                          , gets (dmap-sides B proj₂ s x) y is) ⟹ s
  sides-pair done x y = □
  sides-pair (eq s s₀ s₁) x y = cong-eq (ext \i → sides-pair (s i) _ _) (eqs-pair (s i₀) s₀) (eqs-pair (s i₁) s₁)
    
  eqs-pair : ∀ {a b n}
               {A : Vec I n → Set a}
               {B : (is : Vec I n) → A is → Set b}
               (s : Sides n (\js → Σ (A js) \x → B js x))
               (xy : Eqs s)
           → Meta.subst Eqs (sides-pair s (map-eqs proj₁ s xy) (dmap-eqs B proj₂ s xy))
                   (refls \is → gets (map-sides proj₁ s) (map-eqs proj₁ s xy) is
                              , gets (dmap-sides B proj₂ s (map-eqs proj₁ s xy))
                                     (dmap-eqs B proj₂ s xy) is)
             ⟹ xy
  eqs-pair done xy = □
  eqs-pair {A = A} {B = B} (eq s s₀ s₁) xy =
    cong-refl {A = \js → Σ (A js) \x → B js x}
              {x = \i → refls \is → gets (map-sides proj₁ (s i)) (map-eqs proj₁ (s i) (xy ^ i)) is
                                  , gets (dmap-sides (B ∘∷ i) proj₂ (s i) (map-eqs proj₁ (s i) (xy ^ i)))
                                         (dmap-eqs (B ∘∷ i) proj₂ (s i) (xy ^ i)) is}
              (ext \i → sides-pair (s i) _ _)
              (ext \i → eqs-pair (s i) (xy ^ i))

eqs-si' : ∀ {a b n}
            {A : I → Vec I n → Set a}
            {B : (i : I) → (is : Vec I n) → A i is → Set b}
        → (sides : (i : I) → Sides n (\js → Σ (A i js) \x → B i js x))
        → Eqs (sides i₀)
        → ∀ js → Σ (A i₁ js) \x → B i₁ js x
eqs-si' {A = A} {B = B} s xy0 is =
  let x' = \i' → tr (\i → Eqs (map-sides proj₁ (s (icase i₀ i' i)))) (map-eqs proj₁ (s i₀) xy0) in
  let y' = \i' → tr (\i → Eqs (dmap-sides (B (icase i₀ i' i)) proj₂ (s (icase i₀ i' i)) (x' (icase i₀ i' i))))
                    (dmap-eqs (B i₀) proj₂ (s i₀) xy0) in
  gets (map-sides proj₁ (s i₁)) (x' i₁) is ,
  gets (dmap-sides (B i₁) proj₂ (s i₁) (x' i₁)) (y' i₁) is

eqs-si : ∀ {a b n}
           {A : I → Vec I n → Set a}
           {B : (i : I) → (is : Vec I n) → A i is → Set b}
       → (sides : (i : I) → Sides n (\js → Σ (A i js) \x → B i js x))
       → Eqs (sides i₀)
       → Eqs (sides i₁)
eqs-si s xy0 = Meta.subst Eqs (sides-pair (s i₁) _ _) (refls (eqs-si' s xy0))

postulate tr-eqs-si : ∀ {a b n} {A : I → Vec I n → Set a} -- HIDE a|b
                        {B : (i : I) → (is : Vec I n) → A i is → Set b} -- HIDE a|b
                        (sides : (i : I) → Sides n (\js → Σ (A i js) \x → B i js x))
                        (xy₀ : Eqs (sides i₀))
                    → tr (Eqs ∘ sides) xy₀
                    ⟹ eqs-si sides xy₀
\end{code}

This is the evaluation strategy implemented in the current TTIE interpreter.
But it has two issues:
1) it is error prone and ugly
2) we still haven't defined @tr (Eq Set u v)@

What remains is to define @tr (Eq Set u v)@.

//------------------------------------------------------------------------------
-- A note about transitivity
//------------------------------------------------------------------------------

Note that transitivity can be defined by transporting along an equality,

\begin{code}
trans′ : ∀ {a} {A : Set a} {x y z : A} → x ≡ y → y ≡ z → x ≡ z -- HIDE a
trans′ {y = y} x≡y y≡z = tr (\i → (x≡y ^ inot i) ≡ (y≡z ^ i)) (refl \_ → y)
\end{code}

There are several ways to generalize this to dependent types. I'll use a variant that is explicit about the type

\begin{code}
trans : ∀ {a} (A : I → I → Set a) {x y z} -- HIDE a
      → Eq (\i → A i₀ i) x y
      → Eq (\i → A i i₁) y z
      → Eq (\i → A i i) x z
trans A {y = y} x≡y y≡z = tr (\i → Eq (\j → A (icase i₀ i j) (icase (inot i) i₁ j)) (x≡y ^ inot i) (y≡z ^ i)) (refl \_ → y)
\end{code}

Just as transitivity can be defined in terms of @tr@, the converse is also true.
Instead of specifying transport for nested equality types, we could define @tr@ for @Eq@ types in terms of transitivity and symmetry.

The most general case of such a transport is
> xy = fw (\i → Eq (\j → A i j) (ux ^ i) (vy ^ i)) uv
where
> ux : Eq (\i → A i i₀) u x
> vy : Eq (\i → A i i₁) v y
> uv : Eq (\j → A i₀ j) u v

which we can draw in a diagram as
<svg style="display:block" width="300" height="230">
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="4" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L3,4 L0,8 L9,4 z"/>
    </marker>
  </defs>
  <g transform="translate(150,110)">
    <circle cx="-80" cy="-80" r="4" fill="black"/>
    <circle cx="80" cy="-80" r="4" fill="black"/>
    <circle cx="-80" cy="80" r="4" fill="black"/>
    <circle cx="80" cy="80" r="4" fill="black"/>
    <line x1="-74" y1="-80" x2="74" y2="-80" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
    <line x1="-74" y1="80" x2="74" y2="80" stroke="black" stroke-width="1" marker-end="url(#arrow)" stroke-dasharray="5,5"/>
    <line x1="-80" y1="-75" x2="-80" y2="74" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
    <line x1="80" y1="-74" x2="80" y2="74" stroke="black" stroke-width="1" marker-end="url(#arrow)"/>
    <text x="-80" y="-90" text-anchor="middle">u : A i₀ i₀</text>
    <text x="80" y="-90" text-anchor="middle">v : A i₀ i₁</text>
    <text x="-80" y="90" text-anchor="middle" dominant-baseline="text-before-edge">x : A i₁ i₀</text>
    <text x="80" y="86" text-anchor="middle" dominant-baseline="text-before-edge">y : A i₁ i₁</text>
    <text x="0" y="-90" text-anchor="middle">uv</text>
    <text x="-88" y="0" text-anchor="end" dominant-baseline="middle">ux</text>
    <text x="88" y="0" text-anchor="start" dominant-baseline="middle">vy</text>
  </g>
</svg>

If you ignore the types for now, it seems obvious that
> xy = trans (trans ((sym ux) uv) vy)

So, we could take

\begin{code}
postulate tr-eq : ∀ {a} {A : I → I → Set a} -- HIDE a
                    (ux : ∀ i → A i i₀)
                    (vy : ∀ i → A i i₁)
                    (uv : Eq (A i₀) (ux i₀) (vy i₀))
                → tr (\i → Eq (A i) (ux i) (vy i)) uv
                ⟹ trans (\i j → A (icase i₁ i j) (icase i i j))
                    (refl (ux ∘ inot)) (trans A uv (refl vy))
\end{code}

I will stick to taking @tr@ as primitive. However, this definition will come in handy for defining transport along paths between types.

//------------------------------------------------------------------------------
-- Inductive types
//------------------------------------------------------------------------------

It is straightforward to extend the theory with inductive types and higher inductive types.
Here are some concrete examples, taken from <a href="https://homotopytypetheory.org/book/">the HoTT book</a>.

--- The homotopy circle

\begin{code}
postulate Circle : Set
postulate point  : Circle
postulate loop   : Eq (\_ → Circle) point point
postulate Circle-elim : ∀ {a} {A : Circle → Set a} -- HIDE a
                      → (p : A point)
                      → (l : Eq (\i → A (loop ^ i)) p p)
                      → (x : Circle) → A x
\end{code}
with the computation rules
\begin{code}
postulate elim-point : ∀ {a A p l} → Circle-elim {a} {A} p l point ⟹ p -- HIDE a
postulate elim-loop  : ∀ {a A p l i} → Circle-elim {a} {A} p l (loop ^ i) ⟹ l ^ i -- HIDE a
{-# REWRITE elim-point #-}
{-# REWRITE elim-loop #-}
\end{code}
Technically we would also need to specify @elim@ for transitive paths (or paths constructed with @tr@).
First the non-dependent version,
\begin{code}
postulate Circle-elim′-tr-eq : ∀ {a A p l} (x y : I → Circle) xy i -- HIDE a
            → Circle-elim {a} {\_ → A} p l (tr (\j → x j ≡ y j) xy ^ i) -- HIDE a
            ⟹ tr (\j → Circle-elim {a} {\_ → A} p l (x j) -- HIDE a
                      ≡ Circle-elim {a} {\_ → A} p l (y j)) -- HIDE a
                  (refl \k → Circle-elim {a} {\_ → A} p l (xy ^ k)) ^ i -- HIDE a

\end{code}

To write down the dependent version, it is helpful to first define a generalized version of transport over equality types. 
This generalized equality transport doesn't just give the final path, but also any of the sides, depending on the argument. Fortunately, it can be defined in terms of the existing transport primitive @tr@.

\begin{code}
treq : ∀ {a} (A : I → I → Set a) -- HIDE a
     → (x : ∀ i → A i i₀) (y : ∀ i → A i i₁) (xy : Eq (\j → A i₀ j) (x i₀) (y i₀))
     → (i j : I) → A i j
treq A x y xy i j = tr (\k → Eq (A (i && k)) (x (i && k)) (y (i && k))) xy ^ j
\end{code}
Note that we have
\begin{code}
treq-i-i₀ : ∀ {a} A x y xy i → treq {a} A x y xy i i₀ ⟹ x i -- HIDE a
treq-i-i₁ : ∀ {a} A x y xy i → treq {a} A x y xy i i₁ ⟹ y i -- HIDE a
treq-i₀-j : ∀ {a} A x y xy j → treq {a} A x y xy i₀ j ⟹ xy ^ j -- HIDE a
treq-i₁-j : ∀ {a} A x y xy j → treq {a} A x y xy i₁ j ⟹ tr (\i → Eq (A i) (x i) (y i)) xy ^ j -- HIDE a
-- HIDDEN
treq-i-i₀ A x y xy i = □
treq-i-i₁ A x y xy i = □
treq-i₀-j A x y xy i = □
treq-i₁-j A x y xy i = □
\end{code}

Now the dependent version of commuting @Circle-elim@ for transitive paths looks like this:

\begin{code}
postulate Circle-elim-tr-eq : ∀ {a A p l} (x y : I → Circle) xy i -- HIDE a
            → Circle-elim {a} {A} p l (tr (\j → x j ≡ y j) xy ^ i)
            ⟹ tr (\j → Eq (\k → A (treq _ x y xy j k)) 
                          (Circle-elim {a} {A} p l (x j))
                          (Circle-elim {a} {A} p l (y j)))
                 (refl \k → Circle-elim {a} {A} p l (xy ^ k)) ^ i
\end{code}

We also need to continue this for higher paths, but that should be straightforward, if tedious.

\begin{code}
-- COLLAPSED: tedious next step...
postulate Circle-elim-tr-eq-eq : ∀ {a A p ll} (x y : I → I → Circle) -- HIDE a
                                   (xy₀ : ∀ k → x k i₀ ≡ y k i₀) (xy₁ : ∀ k → x k i₁ ≡ y k i₁)
                                   xy i j
            → Circle-elim {a} {A} p ll (tr (\k → Eq (\l → x k l ≡ y k l) (xy₀ k) (xy₁ k)) xy ^ i ^ j)
            ⟹ tr (\k → Eq (\l → Eq (\m → A (tr (\k' → Eq (\l' → x (k && k') l' ≡ y (k && k') l')
                                                         (xy₀ (k && k'))
                                                         (xy₁ (k && k'))) xy ^ l ^ m) )
                                   (Circle-elim {a} {A} p ll (x k l))
                                   (Circle-elim {a} {A} p ll (y k l)))
                          (refl \l → Circle-elim {a} {A} p ll (xy₀ k ^ l))
                          (refl \l → Circle-elim {a} {A} p ll (xy₁ k ^ l)))
                 (refl \k → refl \l → Circle-elim {a} {A} p ll (xy ^ k ^ l)) ^ i ^ j
\end{code}


--- Truncation

\begin{code}
postulate Truncate : Set → Set
postulate box  : ∀ {A} → A → Truncate A
postulate same : ∀ {A} x y → Eq (\_ → Truncate A) x y

module _ {p} {A} {P : Truncate A → Set p} -- HIDE p
         (b : (x : A) → P (box x))
         (s : ∀ {x y} (px : P x) (py : P y) → Eq (\i → P (same x y ^ i)) px py) where
  
  postulate Truncate-elim : (x : Truncate A) → P x

  postulate elim-box  : ∀ x → Truncate-elim (box x) ⟹ b x
  postulate elim-same : ∀ x y i → Truncate-elim (same x y ^ i)
                                ⟹ s (Truncate-elim x) (Truncate-elim y) ^ i
\end{code}

Notice that in the eliminator for every path constructor, we expect an argument of type @P@ "along that path constructor".

--- Quotient types

\begin{code}
postulate _/_      : (A : Set) → (R : A → A → Set) → Set
postulate quot     : ∀ {A R} → A → A / R
postulate eqn      : ∀ {A R} → (x y : A) → R x y → Eq (\_ → A / R) (quot x) (quot y)
postulate truncate : ∀ {A R} → (x y : A / R) → (r s : Eq (\_ → A / R) x y) → r ≡ s

module _ {A R} {P : A / R → Set}
         (q : (x : A) → P (quot x))
         (e : ∀ {x y} → (r : R x y) → Eq (\i → P (eqn x y r ^ i)) (q x) (q y))
         (t : ∀ {x y r s}
            → (px : P x) (py : P y) (pr : Eq (\i → P (r ^ i)) px py) (ps : Eq (\i → P (s ^ i)) px py)
            → Eq (\i → Eq (\j → P (truncate x y r s ^ i ^ j)) px py) pr ps) where

  postulate /-elim : (x : A / R) → P x

  postulate elim-quot : ∀ x → /-elim  (quot x) ⟹ q x
  postulate elim-eqn  : ∀ x y r i → /-elim (eqn x y r ^ i) ⟹ e r ^ i
  postulate elim-truncate : ∀ x y r s i j
                          → /-elim (truncate x y r s ^ i ^ j)
                          ⟹ t (/-elim x) (/-elim y) (refl \k → /-elim (r ^ k)) (refl \k → /-elim (s ^ k)) ^ i ^ j
\end{code}

--- Indexed types
One caveat to the support of inductive types are indexed types. These are the types with parameters whose value can depend on the constructor, written after the colon in Agda. An obvious example is the standard inductive equality type as it is defined in the standard library,
> data _≡_ {A : Set} (x : A) : A → Set where
>   refl : x ⟹ x
Another example are length indexed vectors,
> data Vec (A : Set) : ℕ → Set where
>   [] : Vec A zero
>   _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)
Such inductive types introduce a new kind of equality, and we can't have that in TTIE.

Fortunately, outlawing such definitions is not a big limitation, since any indexed type can be rewritten to a normal inductive type by making the equalities explicit. For example
> data Vec (A : Set) (n : ℕ) : Set where
>   [] : n ≡ zero → Vec A n
>   _∷_ : ∀ {m} → A → Vec A m → n ≡ suc m → Vec A n


//------------------------------------------------------------------------------
-- Univalence
//------------------------------------------------------------------------------

The final ingredient to turn TTIE into a homotopy type theory is the univalence axiom.
A univalence primitive might look like this:

\begin{code}
postulate univalence : ∀ {a} {A B : Set a} -- HIDE a
                     → (f : A → B)
                     → (g : B → A)
                     → (gf : ∀ x → g (f x) ≡ x)
                     → (fg : ∀ x → f (g x) ≡ x)
                     → (fgf : ∀ x → cong′ f (gf x) ≡ fg (f x))
                     → Eq (\_ → Set a) A B -- HIDE a
\end{code}

By using an equality constructed with univalence in a transport, you can recover the forward and backward functions,

\begin{code}
fw : ∀ {a} {A B : Set a} → A ≡ B → A → B -- HIDE a
fw A≡B = tr (_^_ A≡B)

bw : ∀ {a} {A B : Set a} → A ≡ B → B → A -- HIDE a
bw A≡B = tr (_^_ A≡B ∘ inot)
\end{code}

as well as the proofs of left and right-inverse,

\begin{code}
bw∘fw : ∀ {a} {A B : Set a} → (A≡B : A ≡ B) → ∀ x → bw A≡B (fw A≡B x) ≡ x -- HIDE a
bw∘fw A≡B x = refl \j → tr (\i → A≡B ^ icase (inot j) i₀ i)
                       (tr (\i → A≡B ^ icase i₀ (inot j) i) x)

fw∘bw : ∀ {a} {A B : Set a} → (A≡B : A ≡ B) → ∀ x → fw A≡B (bw A≡B x) ≡ x -- HIDE a
fw∘bw A≡B x = refl \j → tr (\i → A≡B ^ icase j i₁ i)
                       (tr (\i → A≡B ^ icase i₁ j i) x)
\end{code}

Here the trick is that when @j = i₁@, the transports become the identity, while otherwise they become @fw@ and @bw@.

Getting out the adjunction @fgf@ is a bit harder.
You need to come up with an expression that reduces to @f (gf x ^ k)@ when @j = i₀@ and that reduces to @(fg (f x) ^ k)@
 when @j = i₁@.
The following does the trick

\begin{pseudocode}
not-quite-fw∘bw∘fw : ∀ {a} {A B : Set a} → (A≡B : A ≡ B) → ∀ x -- HIDE a
                   → cong′ (fw A≡B) (bw∘fw A≡B x) ≡ fw∘bw A≡B (fw A≡B x)
not-quite-fw∘bw∘fw A≡B x = refl \j →
  refl \k → tr (\i → A≡B ^ icase                          (icase i₀ k j) i₁ i)
          $ tr (\i → A≡B ^ icase    (icase (inot k) i₁ j) (icase i₀ k j)    i)
          $ tr (\i → A≡B ^ icase i₀ (icase (inot k) i₁ j)                   i) x)
\end{pseudocode}

but the type is not right. We want an equality between two equalities, both of type @fw (bw (fw x)) ≡ x@. But instead we get a dependent equality type that mirrors the body of the definition.

To resolve this, we need to add another reduction rule to the language, which states that if you transport from @i₀@ to @i@ and then to @i₁@, this is the same as going directly from @i₀@ to @i₁@. This should hold regardless of what @i@ is.

\begin{code}
postulate tr-tr : ∀ {a} (A : I → Set a) i x → tr (A ∘ icase i i₁) (tr (A ∘ icase i₀ i) x) ⟹ tr A x -- HIDE a
postulate tr-tr-i₀ : ∀ {a} A x → tr-tr {a} A i₀ x ⟹ □ -- HIDE a
postulate tr-tr-i₁ : ∀ {a} A x → tr-tr {a} A i₁ x ⟹ □ -- HIDE a
{-# REWRITE tr-tr-i₀ tr-tr-i₁ #-}
\end{code}

\begin{code}
-- HIDDEN
-- Unfortunately Agda doesn't allow @tr-tr@ as a rewrite rule, so we will need to manually do rewriting under @Eq@ constructors.

cong-Eq : ∀ {a} {A A' x x' y y'} → (AA' : A ⟹ A')
        → Meta.subst id (cong-ap AA' i₀) x ⟹ x'
        → Meta.subst id (cong-ap AA' i₁) y ⟹ y'
        → Eq {a} A x y ⟹ Eq {a} A' x' y'
cong-Eq □ □ □ = □
\end{code}

\begin{code}
fw∘bw∘fw : ∀ {a} {A B : Set a} → (A≡B : A ≡ B) → ∀ x 
         → cong′ (fw A≡B) (bw∘fw A≡B x) ≡ fw∘bw A≡B (fw A≡B x)
fw∘bw∘fw A≡B x = 
-- COLLAPSED: -- same as above, with ugly rewriting details...
  Meta.subst id (cong-Eq
    (ext \j → cong-Eq □ □ (tr-tr (\i → A≡B ^ i) (j) x)) □ □)
    (refl \j → refl \k
          → tr (\i → A≡B ^ icase                          (icase i₀ k j) i₁ i)
          $ tr (\i → A≡B ^ icase    (icase (inot k) i₁ j) (icase i₀ k j)    i)
          $ tr (\i → A≡B ^ icase i₀ (icase (inot k) i₁ j)                   i) x)
\end{code}



// -----------------------------------------------------------------------------
--- Computation rules
// -----------------------------------------------------------------------------

The computation rules are now obvious: when @fw@, @bw@, etc. are applied to a univalence primitive, return the appropriate field.

\begin{code}
module _ {a} {A B} f g gf fg fgf (let AB = univalence {a} {A} {B} f g gf fg fgf) where -- HIDE a
  postulate tr-univalence-f : ∀ x → tr (\i → AB ^ i) x ⟹ f x
  postulate tr-univalence-g : ∀ x → tr (\i → AB ^ inot i) x ⟹ g x
  {-# REWRITE tr-univalence-f #-}
  {-# REWRITE tr-univalence-g #-}

  postulate tr-univalence-gf : ∀ x j
                             → tr (\i → AB ^ icase j i₀ i) (tr (\i → AB ^ icase i₀ j i) x)
                             ⟹ gf x ^ inot j
  postulate tr-univalence-fg : ∀ x j
                             → tr (\i → AB ^ icase j i₁ i) (tr (\i → AB ^ icase i₁ j i) x)
                             ⟹ fg x ^ j
  {-# REWRITE tr-univalence-gf #-}
  {-# REWRITE tr-univalence-fg #-}
  -- tr-univalence-fgf ommitted
\end{code}

Ideally, we would be able to compute @tr@ for @AB ^ f i@ for any function @f@, and even
> tr (\i → AB ^ f!!!<sub>1</sub>!!! i) ∘ ⋯ ∘ tr (\i → AB ^ f!!!<sub>n</sub>!!! i)
But we quickly run into problems. Consider

\begin{code}
  problem : I → I → A → B
  problem j k = tr (\i → AB ^ icase k i₁ i)
              ∘ tr (\i → AB ^ icase j k i)
              ∘ tr (\i → AB ^ icase i₀ j i)
\end{code}

When @j=i₁@, this reduces to

> problem i₁ k = fg ^ k ∘ f

and when @k=i₀@, it reduces to

> problem j i₀ = f ∘ gf ^ j

These two types look a lot like the adjunction @fgf@,
but there are two differences:
1. For the two reductions of @problem@ to be confluent, the two right hand sides should be equal in the meta language (judgementally equal). But an adjunction inside the theory doesn't guarantee this.

2. Even when using @fgf@, we can not get an expression for @problem@ with the right reductions.
   The issue is that depending on @j@ and @k@, @problem@ can represent any of the following compositions
> -- BLOCK: pseudocode
> problem i₀ i₀ = f  ∘ id ∘ id
> problem i₀ i₁ = id ∘ f  ∘ id
> problem i₁ i₀ = f  ∘ g  ∘ f
> problem i₁ i₁ = id ∘ id ∘ f


// -----------------------------------------------------------------------------
--- Transporting univalent paths
// -----------------------------------------------------------------------------

Finally, we also need to decide how to transport along equality types involving univalence.
As I showed previously, transporting along equalities can be defined in terms of transitivity. So that is what we will do here.
The idea is that to transport along @trans AB BC@, you first transport along @AB@, and then along @BC@. The same goes for other directions of using this transitive path (@bw@, @fw∘bw@, etc.)

\begin{code}
module _ {a} {A B C : Set a} (A≡B : A ≡ B) (B≡C : B ≡ C) where
  trans-f : A → C
  trans-f = fw B≡C ∘ fw A≡B

  trans-g : C → A
  trans-g = bw A≡B ∘ bw B≡C

  trans-gf : ∀ x → trans-g (trans-f x) ≡ x
  trans-gf x = cong′ (bw A≡B) (bw∘fw B≡C (fw A≡B x)) ⟨ trans′ ⟩ bw∘fw A≡B x

  trans-fg : ∀ x → trans-f (trans-g x) ≡ x
  trans-fg x = cong′ (fw B≡C) (fw∘bw A≡B (bw B≡C x)) ⟨ trans′ ⟩ fw∘bw B≡C x

  postulate trans-fgf : ∀ x → cong′ trans-f (trans-gf x) ≡ trans-fg (trans-f x)
  -- trans-fgf should be provable, but proof is omitted here

  trans-equivalence : A ≡ C
  trans-equivalence = univalence trans-f trans-g trans-gf trans-fg trans-fgf
\end{code}

And we use this transitivity to define transport,

\begin{code}
postulate tr-eq-Set : ∀ {a} (A B : I → Set a) (A₀≡B₀ : A i₀ ≡ B i₀)
                    → tr (\i → Eq (\_ → Set a) (A i) (B i)) A₀≡B₀
                    ⟹ trans-equivalence (refl (A ∘ inot)) (trans-equivalence A₀≡B₀ (refl B))

-- spacial case for fw
tr-tr-eq-Set : ∀ {a} (A B : I → Set a) (A₀≡B₀ : A i₀ ≡ B i₀) x
             → tr (\j → tr (\i → Eq (\_ → Set a) (A i) (B i)) A₀≡B₀ ^ j) x
             ⟹ tr B (tr (_^_ A₀≡B₀) (tr (A ∘ inot) x))
tr-tr-eq-Set A B A₀≡B₀ x = Meta.cong (\A₁≡B₁ → tr (_^_ A₁≡B₁) x) (tr-eq-Set A B A₀≡B₀)
\end{code}

Note that @tr-eq-Set@ cannot be used as a rewrite rule. Agda incorrectly complains about universe levels, and when removing those the rule is accepted, but the file takes more than 10 minutes to type check.


// -----------------------------------------------------------------------------
--- Reduction rules spoiled by univalence
// -----------------------------------------------------------------------------

While we are at it, it would be nice if we could add some additional judgemental equalities to the type system.
For instance, @trans xy (sym xy) = refl \_ → x@ should hold for all @xy@.

However, we can not add this as a reduction.
The reason is that for paths build with univalence, transporting along the left hand side reduces to @bw∘fw@, and this is not necessarily the same as reflexivity. Here is an example

\begin{code}
-- A path that flips the interval in one direction, but not in the other
-- so fw ∘ bw ≠ refl
flip-I : I ≡ I
flip-I = univalence id inot
  (\i → refl (icase (inot i) i))
  (\i → refl (icase (inot i) i))
  (\i → refl \_ → refl (icase (inot i) i))

module _ (trans-sym : ∀ {a A x y} xy → trans′ {a} {A} {x} {y} xy (sym xy) -- hide {a}
                                     ⟹ (refl \_ → x)) where
  problem2 : i₀ ⟹ i₁
  problem2 = 
    Meta.begin
      i₀
    ⟸⟨ tr-tr-eq-Set (_^_ flip-I ∘ inot) (_^_ flip-I ∘ inot) (refl \_ → I) i₁ ⟩
      tr (\i → trans′ flip-I (sym flip-I) ^ i) i₁
    ⟹⟨ Meta.cong (\AB → tr (\i → AB ^ i) i₁) (trans-sym flip-I) ⟩
      i₁
    ∎
\end{code}

@tr (\i → trans flip-I (sym flip-I) ^ i) i₁@ evaluates to @i₁@ with @tr-eq-Set@, since we follow the equivalence backward and then forward. But according to @trans-sym@ it is an identity path, and so this expression evaluates to @i₀@.
So, we have a term that can evaluate to either @i₀@ or to @i₁@, depending on the evaluation order. In other words, reduction is no longer confluent.

This might not seem too bad, since @i₀ ≡ i₁@ inside the theory.
But note that the reduction relation @⟹@ is not a homotopy equality. And it might even be untyped if we were using an untyped meta-theory, like the Haskell TTIE implementation. With a non-confluent reduction relation, it is easy to break the type system,

\begin{code}
-- HIDDEN
open import Data.Bool
open import Data.Empty

not-not : ∀ x → not (not x) ≡ x
not-not false = refl \_ → false
not-not true  = refl \_ → true

not-not-not : ∀ x → cong′ not (not-not x) ≡ not-not (not x)
not-not-not false = refl \_ → refl \i → true
not-not-not true  = refl \_ → refl \i → false
\end{code}

\begin{code}
flip-Bool : Bool ≡ Bool
flip-Bool = univalence not not not-not not-not not-not-not

bad : i₀ ⟹ i₁ → (Bool , false) ⟹ (_,_ {B = id} Bool true)
bad x = Meta.cong {B = Σ Set id} (\i → flip-Bool ^ i , tr (\j → flip-Bool ^ i && j) false) x

worse : i₀ ⟹ i₁ → ⊥
worse x with bad x
... | ()
\end{code}


So, @trans-sym@ is out.

Another seemingly sensible reduction is that @cong f (trans xy yz) ≡ trans (cong f xy) (cong f yz)@.
But, if we also postulate that all paths over the interval can be defined in terms of @icase@, we end up in the same problematic situation.

\begin{code}
module _ (trans-cong : ∀ {a b A B x y z} (f : A → B) xy yz -- HIDE a|b
                     → cong′ f (trans′ {a} {A} {x} {y} {z} xy yz) -- HIDE a
                     ⟹ trans′ {b} (cong′ f xy) (cong′ f yz)) -- HIDE b
         (tr-eq-I : ∀ (j k : I → I) jk₀ → tr (\i → Eq (\_ → I) (j i) (k i)) jk₀
                                        ⟹ refl (icase (j i₁) (k i₁))) where
  trans-sym : ∀ {a A x y} xy → trans′ {a} {A} {x} {y} xy (sym xy) ⟹ (refl \_ → x) -- HIDE a
  trans-sym {x = x} xy =
    Meta.begin
      trans′ xy (sym xy)
    ⟸⟨ trans-cong (_^_ xy) (refl id) (refl inot) ⟩
      cong′ (\i → xy ^ i) (trans′ (refl id) (refl inot))
    ⟹⟨ Meta.cong (cong′ (_^_ xy)) (tr-eq-I inot inot (refl \_ → i₁)) ⟩
      refl (\_ → x)
    ∎

  problem3 : i₀ ⟹ i₁
  problem3 = problem2 trans-sym
\end{code}

I don't have any solution to these problems, aside from not adding the problematic reductions.

Reductions that do seem fine are those involving only a single path. For instance, things like @trans xy (refl \_ → y) ⟹ xy@.


//------------------------------------------------------------------------------
-- Conclusion
//------------------------------------------------------------------------------

What I have presented is the type theory with indexed equality.
As mentioned before, there is also a <a href="https://github.com/twanvl/ttie">prototype implementation in Haskell</a>.

The theory is quite similar to the cubical system, but it is developed mostly independently.

Some area's I haven't discussed or investigated yet, and some issues with the theory are:

1. Transitive paths involving HIT path constructors are not reduced, so @trans loop (sym loop)@ is not the same as @refl \_ → point@, however, the two are provably equal inside the theory. As with a general @trans-sym@ rule, adding such a reduction would break confluence.

2. I have defined a function @treq@ that generalizes @tr (Eq ..)@. This could be taken as a primitive instead of @tr@. In that case we should further generalize it to take @Sides@, so that it also works for higher paths. 

3. It is possible to combine transports to write terms that do not reduce, for example

> x : A
> AB : A ≡ B
> f : A → Set
> y : f (bw AB (fw AB x))
> tr (\i → f (tr (\j → AB ^ icase (inot i) i₀ j)
>            (tr (\j → AB ^ icase i (inot i) j)
>            (tr (\j → AB ^ icase i₀ i j) x)))) y

the @tr-tr@ rule handles one such case, but more are possible.
For well-behaved equalities flatting all this out is not a problem, but with univalence the intermediate steps become important.

4. I am not entirely happy with univalence breaking confluence in combination with @trans-sym@. It means that you have to be really careful about what, seemingly benign, reductions are allowed.

