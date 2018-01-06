title: Knight in n, part 3: rings
subtitle: How many ways are there to move a knight on a chessboard from a to b?
date: 2008-12-04
tags: haskell, knight-in-n
sourcelink: This post is literate Haskell, click here to download the source code.

Previously in this series:
* <a href="http://twanvl.nl/blog/haskell/Knight1">part 1: moves</a>
* <a href="http://twanvl.nl/blog/haskell/Knight2">part 2: combinatorics</a>

In this third installment, we will look at how to use various types as numbers, i.e. how to make them an instance of the @Num@ type class.
The solution the Knight-moves-problem will emerge at the end, almost as if by magic. $:)$

> -- HIDDEN
> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Knight3 where
> import Knight1
> import Data.Array

== Tangent: Things as numbers ==

Many types can be used as if they are numbers. Haskell-wise this means they can be an instance of the @Num@ type class. Mathematically it means that these types are <a href="http://en.wikipedia.org/wiki/Ring_(algebra)">rings</a>.

-- Pairs as numbers --

Let's start with a @Num@ instance for pairs @(α,β)@.
In general, our only choice is to do everything pointwise.
So for all operations &otimes; (i.e. @(+)@, @(-)@ and @(*)@:

FORMULA: knight/pointwise_tuple
	\begin{pmatrix}a\\b\end{pmatrix}
	 \otimes
	\begin{pmatrix}c\\d\end{pmatrix}
	 =
	\begin{pmatrix}a\otimes c\\b \otimes d\end{pmatrix}

In ring theory this is called the <a href="http://en.wikipedia.org/wiki/Direct_product_(ring_theory)">''direct product''</a>. In Haskell we can write it as:

> instance (Num α, Num β) => Num (α,β) where
>     (a,b) + (c,d) = (a+c,b+d)
>     (a,b) - (c,d) = (a-c,b-d)
>     (a,b) * (c,d) = (a*c,b*d)
>     fromInteger i = (fromInteger i, fromInteger i)
>     abs     (a,b) = (abs    a, abs    b)
>     signum  (a,b) = (signum a, signum b)

We could also make instances for triples, quadruples and other tuples this way, but those are not needed for the rest of the story.

-- Arrays as numbers --

A more general kind of tuple is an array; which is somewhat like a tuple of arbitrary size.
Of course, that is not quite true, since two arrays with the same type can have a ''different'' size.
One way around this problem is to treat all arrays as if they are infinite, by taking values outside the bounds to be equal to $0$. So
]> -- EXAMPLE
]> listArray (0,0) [1] == listArray (-!!!&infin;!!!,!!!&infin;!!!) [..,0,0,1,0,0,..] -- pseudocode

That way we can still do addition pointwise,
FORMULA: knight/pointwise_add_array
	\begin{pmatrix}a\\b\\\vdots\end{pmatrix}
	 +
	\begin{pmatrix}c\\d\\\vdots\end{pmatrix}
	 =
	\begin{pmatrix}a+c\\b+d\\\vdots\end{pmatrix}

The @accumArray@ function can help with the missing elements by setting them to @0@ by default:

> addArray a b = accumArray (+) 0 (min a__lo b__lo, max a__hi b__hi) (assocs a ++ assocs b)
>   where (a__lo,a__hi) = bounds a
>         (b__lo,b__hi) = bounds b

Next up is the @fromInteger@ function.
@fromInteger 0@ is easy; there are two options for other values

.# @fromInteger i@ is an infinite array of values @i@.
.# @fromInteger i@ is an array with values @i@ at some single point.

The first choice mimics the definition for tuples, @fromInteger i = (fromInteger i, fromInteger i)@.
But for arrays this has the slight problem of requiring an infinite array.
For the second alternative we need to pick the index where to put the number @i@. The obvious choice is to put @i@ at 'the origin', index @0@:

> intArray i = listArray (0,0) [fromInteger i]


Finally, multiplication. As you have learned in school, multiplication can be seen as repeated addition, In our Haskell world that means that we expect the law @a + a = fromInteger 2 * a@ to hold.

If we had used the first choice for @fromInteger@ then multiplication could be done pointwise as it was for tuples. But we have made a different choice, so now @fromInteger 2@ is an array that contains the value @2@ at index @0@ (and is implicitly zero everywhere else). When calculating @fromInteger 2 * a@, this @2@ should by multiplied with ''all'' elements of the array @a@.

The operation that does the right thing is <a href="http://en.wikipedia.org/wiki/Convolution">''convolution''</a>. It looks like this:

FORMULA: knight/pointwise_mul_array
	\begin{pmatrix}a\\b\\c\end{pmatrix}
	 *
	\begin{pmatrix}d\\e\\f\end{pmatrix}
	 \; = \;
	\raisebox{5mm }{$a\begin{pmatrix}d\\e\\f\end{pmatrix}$} +
	\raisebox{0mm }{$b\begin{pmatrix}d\\e\\f\end{pmatrix}$} +
	\raisebox{-5mm}{$c\begin{pmatrix}d\\e\\f\end{pmatrix}$}
	 \; = \;
	\left(\begin{array}{@{}c@{}c@{}c@{}c@{}c@{}}
	     ad&&&&\\ae&+&bd&&\\af&+&be&+&cd\\&&bf&+&ce\\&&&&cf
	\end{array}\right)

So for each element @v@ at index @i@ in the first array, we shift a copy of the second array so that its origin becomes @i@. This copy is multiplied by @v@ and all these copies are added.
If one of the arrays is @fromInteger v@ (i.e. a scalar), then this corresponds to multiplying all elements in the other array by @v@; exactly what we wanted.

Convolution can be implemented with @accumArray@ as:

> mulArray a b
>     = accumArray (+) 0 (bounds a + bounds b)
>       [ (i+j, x*y) | (i,x) <- assocs a, (j,y) <- assocs b ]

Notice that we use the @Num (α,β)@ instance for the bounds, and that this definition is nicely symmetrical.


Putting it all together, we get the following instance:

> instance (Ix i, Num i, Num a) => Num (Array i a) where
>     fromInteger = intArray
>     (+)         = addArray
>     (*)         = mulArray
>     negate      = fmap negate
>     abs         = fmap abs
>     signum      = fmap signum

In mathematical terms, what we constructed here is called a <a href="http://en.wikipedia.org/wiki/Group_ring">''group ring''</a>. There is a group ring $G[R]$ for any group $G$ and ring $R$, which corresponds to an instance  @Num (Array g r)@ when @g@ is a group (i.e. an instance of @Num@) and @r@ is a ring (also an instance of @Num@).

-- Arrays as polynomials --

Another way to interpret the above instance, is by treating arrays as polynomials over some variable $x$. The array @array [(i,a),(j,b),(k,c),..]@ then represents the polynomial $ax^i+bx^j+cx^k+...$. The addition and multiplication defined above now have the expected meaning, for example:
]> > let a = listArray (0,2) [2,3,4]  --  2 + 3x + 4x^2
]> > let b = listArray (1,2) [5,6]    --  5x + 6x^2
]> > a + b
]> array (0,2) [(0,2),(1,8),(2,10)]   --  2 + 8x + 10x^2
]> > a * b
]> array (1,4) [(1,10),(2,27),(3,38),(4,24)]  --  10x + 27x^2 + 38x^3 + 24x^4

We can make this even more suggestive by defining:

> x = listArray (1,1) [1]

]> > (2 + 3*x + 4*x^2) * (5*x + 6*x^2) == 10*x + 27*x^2 + 38*x^3 + 24*x^4
]> True

If you are interested in this interpretation,
sigfpe wrote <a href="http://sigfpe.blogspot.com/2007/11/small-combinatorial-library.html">an interesting blog post</a> about convolutions, polynomials and power series.

== It's magic! ==

Now, let's go back to our original problem, the moves of a chess knight.

The positions reachable in a single move can be put into a two dimensional array (i.e. a matrix).

> moveMatrix :: Array (Int,Int) Integer
> moveMatrix = accumArray (+) 0 ((-2,-2),(2,2)) [ (m,1) | m <- moves ]

This is the familiar ''move matrix'', which we already saw in part 1.
]> Knight3> printMatrix moveMatrix
]     0 1 0 1 0
]     1 0 0 0 1
]     0 0 0 0 0
]     1 0 0 0 1
]     0 1 0 1 0

Now the magic.
We defined multiplication of two arrays @a@ and @b@ as adding copies of @b@ for each value in @a@.
If we use the move matrix as @b@, then this means we add all possible destinations of a knight making one move from each place it can reach. Repeating this $n$ times gives us our answer. Since repeated multiplication is exponentiation:

> allPaths__conv n = moveMatrix ^ n

For example, for $n=2$:<br>
<img src="image/knight/convolution2.png" alt="moveMatrix * moveMatrix" style="margin-left:2em;">

If we are interested in just a single point there is the array indexing operator (!!) to help us,

> paths__conv n ij
>     | inRange (bounds m) ij = m ! ij
>     | otherwise             = 0
>   where m = allPaths__conv n

This convolutional algorithm can count the number of paths in $O(n^3)$, but not just for a single end point, but for ''all'' end points at once! The program is also a lot simpler than the 

The @paths__conv@ algorithm is pretty good, but we can ''still'' do better.
Next time I will show how the algorithm from part 3 can be improved further, and curiously, how it will start to look more like the algorithm from part 2.
