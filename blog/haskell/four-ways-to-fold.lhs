title: Four ways to fold an array
subtitle: foldr and foldl in two ways each
date: 2009-11-09
tags: haskell
sourcelink: This post is literate Haskell, click here to download the source code.

> -- IGNORE
> {-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
> import Data.Array

As most Haskell programmers know, there are two ways to fold a list:
from the right with @foldr@
and from the left with @foldl@.
@foldr@ is corecursive (productive), which is great when the output can be produced lazily.
@foldl@ (or better, its strict cousin @foldl'@) is tail recursive, preventing stack overflows.

We can define analogous operations for other data structures like 1-dimensional arrays.
Libraries like 'Data.ByteString' and 'Data.Vector' provide these.
But as I will show in this post there are more fold operations than the common two.

The data type I will use in this post is simply

> type Array1D a = Array Int a
> -- and two utility functions for getting the lower and upper bounds
> lo,hi :: Array1D a -> Int
> lo = fst . bounds
> hi = snd . bounds

The right fold applies a function @f@ to the current value and the folded result of the rest of the array:

> foldr__a :: (a -> b -> b) -> b -> Array1D a -> b
> foldr__a f z0 ar = go (lo ar)
>   where go i
>          | i > hi ar = z0
>          | otherwise = f (ar ! i) (go (i + 1))

The (strict) left fold uses an accumulator parameter:

> -- IGNORE, this function is the same as foldl' which is more interesting anyway
> foldl__a :: (b -> a -> b) -> b -> Array1D a -> b
> foldl__a f z0 ar = go z0 (lo ar)
>   where go z i
>          | i > hi ar = z
>          | otherwise = go (f z (ar ! i)) (i + 1)

> foldl'__a :: (b -> a -> b) -> b -> Array1D a -> b
> foldl'__a f z0 ar = go z0 (lo ar)
>   where go !z i
>          | i > hi ar = z
>          | otherwise = go (f z (ar ! i)) (i + 1)

In each case, the recursive @go@ function is very similar in structure to the list version;
only instead of recursing for the tail of the list we recurse for index @i+1@.
The time and space behavior is also similar.
For example, if you have a large array

> testArray :: Array1D Integer
> testArray = listArray (1,10^6) [1..]

Then for computing something like the sum of all elements, you should use a strict left fold:

]> *Main> foldl'__a (+) 0 testArray
] 50000005000000
]> *Main> foldr__a (+) 0 testArray
] *** Exception: stack overflow

On the other hand, a right fold is the way to go when you are only interested in a part of a lazily produced result. For example when converting an array to a list:

]> *Main> take 10 . foldr__a (:) [] $ testArray
] [1,2,3,4,5,6,7,8,9,10]
] (0.02 secs, 520824 bytes)
]> *Main> take 10 . foldl'__a (flip (:)) [] $ testArray
] [1000000,999999,999998,999997,999996,999995,999994,999993,999992,999991]
] (5.89 secs, 263122464 bytes)

All of this is exactly the same as with lists.


<br>
But, if you look at @foldr__a@ and @foldl'__a@, you will see that they both contain a loop doing @(i + 1)@.
So in a sense, both of these functions work from left to right!

Because arrays allow for random access, it is possible to make true right to left folds,
just start at the end and do @(i - 1)@ in each iteration.

> foldl__b :: (b -> a -> b) -> b -> Array1D a -> b
> foldl__b f z ar = go (hi ar)
>   where go i
>          | i < lo ar = z
>          | otherwise = f (go (i - 1)) (ar ! i)

> foldr'__b :: (a -> b -> b) -> b -> Array1D a -> b
> foldr'__b f z0 ar = go z0 (hi ar)
>   where go !z i
>          | i < lo ar = z
>          | otherwise = go (f (ar ! i) z) (i - 1)

Just look at the pretty duality there! We now have a ''lazy'' left fold and a ''strict'' right fold.


The behavior is exactly the opposite of that of the @fold__a@ functions above:

]> *Main> foldl__b (+) 0 testArray
] *** Exception: stack overflow
]> *Main> foldr'__b (+) 0 testArray
] 50000005000000

]> *Main> take 10 . foldr'__b (:) [] $ testArray
] [1,2,3,4,5,6,7,8,9,10]
] (6.19 secs, 263055372 bytes)
]> *Main> take 10 . foldl__b (flip (:)) [] $ testArray
] [1000000,999999,999998,999997,999996,999995,999994,999993,999992,999991]
] (0.00 secs, 524836 bytes)

To summarize, four ways to fold an array are:
<table>
<tr><td></td><td>@lo@ to @hi@, @i+1@</td><td>@hi@ to @lo@, @i-1@</td></tr>
<tr><td>corecursion, productive, lazy</td><td>@foldr__a@</td><td>@foldl__b@</td><tr>
<tr><td>accumulator, tail recursive, strict</td><td>@foldl'__a@</td><td>@foldr'__b@</td><tr>
</table>

Exercise: can you think of other ways to fold an array?
