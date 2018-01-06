title: Knight in n, part 4: tensors
subtitle: How many ways are there to move a knight on a chessboard from a to b?
date: 2008-12-10
tags: haskell, knight-in-n
sourcelink: This post is literate Haskell, click here to download the source code.

Previously in this series:
* <a href="http://twanvl.nl/blog/haskell/Knight1">part 1: moves</a>
* <a href="http://twanvl.nl/blog/haskell/Knight2">part 2: combinatorics</a>
* <a href="http://twanvl.nl/blog/haskell/Knight3">part 3: rings</a>

> -- HIDDEN
> module Knight4 where
> import Knight1
> import Knight2
> import Knight3
> import Debug.SimpleReflect hiding (a,b,x)
> import Data.Array

Welcome to the fourth installement of the ''Knight in n'' series.
In part 3 we talked about the direct product of rings, and how they helped us solve the knight moves problem.
This time yet another type of product is going to help in decomposing the algorithm to allow faster parts to be put in.


== The tensor product of rings ==

In part three I introduced the direct product on rings, which is nothing more than a pair of numbers.
Confusingly this operation is also called <a href="http://en.wikipedia.org/wiki/Direct_sum">direct ''sum''</a>.
To illustrate this name, take the direct sum/product of @Array i a@ with @Array j b@.
For every index @i@ (within the bounds of the first array) there is a value of type @a@, and for every index @j@ there is a value of type @b@.
Instead of a pair of arrays, this could also be implemented as a single array
with the type @Array (Either i j) (Either a b)@. "Either" is just Haskell's way of saying "disjoint union" or "sum type", hence "direct ''sum''".

There is another product operation that we can perform on two rings: the <a href="http://en.wikipedia.org/wiki/Tensor_product">tensor product</a>.
Dually to the direct sum, the tensor product of @Array i a@ and @Array j b@ has type @Array (i,j) (a,b)@.
The definition is very simple: the array contains all pairs where the first part comes from the first array, and the second part comes from the second array.

Slightly more generally, we can use any combining function. The general tensor product of two arrays can be implemented as:

> tensorWith :: (Ix i, Ix j) => (a -> b -> c) -> Array i a -> Array j b -> Array (i,j) c
> tensorWith f a b
>     = array ((a__lo,b__lo),(a__hi,b__hi))
>       [ ((i,j), f x y) | (i,x) <- assocs a, (j,y) <- assocs b ]
>   where (a__lo,a__hi) = bounds a
>         (b__lo,b__hi) = bounds b

Usually elements are multiplied:

> (><) :: (Ix i, Ix j, Num a) => Array i a -> Array j a -> Array (i,j) a
> (><) = tensorWith (*)

The mathematical notation for this @(><)@ operator is &otimes;.
Now an example:
Here we take two $4$-element vectors, their tensor product has $4*4=16$ elements.
The two vectors are "one dimensional<a href="#footnote-dimension" name="footnote-dimension-back">*</a>" objects, their tensor product is a "two dimensional" matrix.

FORMULA: knight/tensor-product1
	\begin{pmatrix}
	 1\\2\\3\\4
	\end{pmatrix}
	 \otimes
	\begin{pmatrix}
	 a\\c\\d\\e
	\end{pmatrix}
	 = 
	\begin{pmatrix}
	 1a & 1b & 1c & 1d \\
	 2a & 2b & 2c & 2d \\
	 3a & 3b & 3c & 3d \\
	 4a & 4b & 4c & 4d \\
	\end{pmatrix}


A special case we will use often is the tensor product of an array with itself:

> square x = x >< x

For example (using <a href="http://twanvl.nl/blog/haskell/simple-reflection-of-expressions">simple reflection of expressions</a> which is now on hackage as <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/simple-reflect">Debug.SimpleReflect</a>):

]> Knight4> square (listArray (0,2) [u,v,w])
]> listArray ((0,0),(2,2)) [u*u, u*v, u*w
]>                         ,v*u, v*v, v*w
]>                         ,w*u, w*v, w*w]


== Interchange law ==

The tensor product and convolution operations satisfy the very useful ''interchange law'':
FORMULA: knight/tensor-interchange
	(a \otimes b) * (c \otimes d) = (a * c) \otimes (b * d)
!!!style="margin-top:.1em"!!!And since exponentiation is repeated convolution, also
FORMULA: knight/tensor-interchange-exponent
	(a \otimes b)^n = a^n \otimes b^n

For a proof sketch of this equation,
compare the definitions of @(><)@ and @mulArray@. Ignoring array bounds stuff, we have:
]> convolution:     [ ( !!!<strong>!!!i+j!!!</strong>!!!,  x*y) | (i,x) <- assocs a, (j,y) <- assocs b ]
]> tensor product:  [ (!!!<strong>!!!(i,j)!!!</strong>!!!, x*y) | (i,x) <- assocs a, (j,y) <- assocs b ]
The only difference is in what happens to indices, with convolution the indices are added, with the tensor product a pair is formed. Now consider the interchange law. Informally, the indices of the left hand side are of the form @(i__a,i__b)+(i__c,i__d)@, and on the right hand side @(i__a+i__c,i__b+i__d)@. This corresponds exactly to the piecewise addition for @Num (α,β)@.

The interchange law is often exploited to perform faster convolutions.
For example, consider blurring an image by taking the convolution with a Gaussian blur kernel:
<br><img src="image/knight/convolution-blur1.png" alt="image*blur=blurred_image" style="margin-left:2em;margin-top:2px;">
<br>Performing this convolution requires $O(n^4)$ operations for an $n$ by $n$ image.

The two dimensional Gaussian blur kernel can be written as the tensor product of two one dimensional kernels,
with a bit algebra this gives:
<br><img src="image/knight/convolution-blur2.png" alt="" style="margin-left:0;margin-top:2px;">

So now to blur an image we can perform two convolution, first with the horizontal kernel, and then with the vertical one:
<br><img src="image/knight/convolution-blur3.png" alt="image*blurH*blurV=blurred_image" style="margin-left:2em;margin-top:2px;">
<br>This procedure needs only $O(n^3)$ operations.


== Back to business ==

Blurring images is not what we are trying to do.
Instead of convolution with the Gaussian blur kernel, we are interested in convolution with @moveMatrix@.
We could try the same trick, finding an @a@ such that @moveMatrix == a >< a@.
Unfortunately, this is impossible.

But we can still get close, there ''is'' a way to write @moveMatrix == square a + square b@, well, almost.
Actually, what we have is:
]> 2 * moveMatrix
]       0 2 0 2 0        1 1 0 1 1      1 -1  0 -1  1 
]       2 0 0 0 2        1 1 0 1 1     -1  1  0  1 -1 
]  ==   0 0 0 0 0   ==   0 0 0 0 0  -   0  0  0  0  0   ==   square a - square b
]       2 0 0 0 2        1 1 0 1 1     -1  1  0  1 -1 
]       0 2 0 2 0        1 1 0 1 1      1 -1  0 -1  1 

where

> a,b :: Array Int Integer
> a = listArray (-2,2) [1,1,0,1,1]
> b = listArray (-2,2) [1,-1,0,-1,1]


Now we can start with @paths__conv@ from last time:

> paths__conv n ij = (moveMatrix ^ n) `safeAt` ij

Where @safeAt@ is a safe array indexing operator, that returns @0@ for indices that are out of bounds:

> safeAt ar i
>     | inRange (bounds ar) i = ar ! i
>     | otherwise             = 0

Now let's do some algebraic manipulation:

]>     paths__conv n ij
]> = {- by definition of paths__conv -}
]>     (moveMatrix ^ n) `safeAt` ij
]> = {- by defintion of a and b -}
]>     ((square a - square b) `div` 2)^n `safeAt` ij -- division by 2 is pseudocode
]> = {- division does not depend on the index -}
]>     (square a - square b)^n `safeAt` ij `div` 2^n

We still cannot apply the interchange law, because the exponentiation @(^n)@ is applied to the difference of two tensor products and not a single one.
We can, however, expand this exponentation by the formula:

]> (a + b)^n = sum [ multinomial [n__a,n__b] * a^n__a * b^n__b | (n__a,n__b) <- split n ]

This is just the usual <a href="http://en.wikipedia.org/wiki/Binomial_theorem">binomial expansion</a>, as in
FORMULA: knight/binomial-expansion-2
	(x+y)^2 &= x^2 + 2xy + y^2\\
	(x+y)^3 &= x^3 + 3x^2y + 3xy^2 + y^3\\
	\text{etc.}

Applying binomial expansion to our work-in-progress gives:

]>     (square a - square b)^n `safeAt` ij `div` 2^n
]> = {- binomial expansion -}
]>     sum [ multinomial [n__a,n__b] * square a^n__a * (-square b)^n__b
]>         | (n__a,n__b) <- split n ]
]>     `safeAt` ij `div` 2^n
]> = {- (-square b)^n__b == (-1)^n__b * square b^n__b -}
]>     sum [ multinomial [n__a,n__b] * (-1)^n__b
]>         * square a^n__a * square b^n__b
]>         | (n__a,n__b) <- split n ]
]>     `safeAt` ij `div` 2^n
]> = {- interchange law -}
]>     sum [ multinomial [n__a,n__b] * (-1)^n__b
]>         * square (a^n__a * b^n__b)
]>         | (n__a,n__b) <- split n ]
]>     `safeAt` ij `div` 2^n
]> = {- move `safeAt` inwards, since addition is pointwise -}
]>     sum [ multinomial [n__a,n__b] * (-1)^n__b
]>         * square (a^n__a * b^n__b) `safeAt` ij
]>         | (n__a,n__b) <- split n ]
]>     `div` 2^n

== Fast indexing ==

Since @square !!!<em>!!!something!!!</em>!!!@ already has $n^2$ elements and the loop is performed $n+1$ times,
this algorithm still requires $O(n^3)$ operations.

The only reason for calculating @square (a^n__a * b^n__b)@ is because we need the element at index @ij@.
So instead of constructing a whole array, let's just calculate that single element:

> -- square x `safeAt` ij  ==  x `squareAt` ij
> x `squareAt` (i,j) = x `safeAt` i * x `safeAt` j

So the inner part of the algorithm becomes:
]>     square (a^n__a * b^n__b) `safeAt` ij
]> = {- property of squareAt -}
]>     (a^n__a * b^n__b) `squareAt` ij

We are still not there yet. Both @a^n__a@ and @b^n__b@ have $O(n)$ elements, so just calculating their convolution takes $O(n^2)$ work. But again, we need only two elements of the convolution, so we can define:

> -- a * b `safeAt` i  ==  mulArrayAt a b i
> mulArrayAt a b n = sum [ x * b `safeAt` (n-i) | (i,x) <- assocs a ]

And update @squareAt@ accordingly:

> mulSquareAt a b (i,j) = mulArrayAt a b i * mulArrayAt a b j

Finally we need a more efficient way to calculate all the powers of @a@ and @b@.
The @iterate@ function can help us with that:
]> Knight4> iterate (*u) 1
]> [1, 1*u, 1*u*u, 1*u*u*u, 1*u*u*u*u, ...

Putting the pieces together gives a $O(n^2)$ algorithm for the knight moves problem:

> paths__tensor n ij
>       = sum [ multinomial [n__a,n__b] * (-1)^n__b
>             * mulSquareAt (powers_of_a !! n__a) (powers_of_b !! n__b) ij
>             | (n__a,n__b) <- split n
>             ]
>        `div` 2^n
>   where powers_of_a = iterate (*a) 1
>         powers_of_b = iterate (*b) 1

Note that the savings we have made do not come directly from decomposing the @moveMatrix@. It is just that this decomposition allows us to see that we are computing all elements of am expensive product where a single one would do.

This post brings another order of improvement.
Do you think you can do better than $O(n^2)$ time and $O(n^2)$ space complexity? If so I would like to hear.



<br><a name="footnote-dimension" href="#footnote-dimension-back">*</a>: The number of elements is often called the dimension of a vector. Here we use the term dimension to refer to the number of indices used, also known as the <a href="http://en.wikipedia.org/wiki/Tensor_order#Tensor_rank">(tensor) order</a>. So a $100*100$ pixel image has dimension $10000$ according to the first interpretation (the number of elements), but dimension two in the second interpretation (the number of indices).