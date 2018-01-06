title: Knight in n, part 2: combinatorics
subtitle: How many ways are there to move a knight on a chessboard from a to b?
date: 2008-12-01
tags: haskell, knight-in-n
sourcelink: This post is literate Haskell, click here to download the source code.

Previously in this series:
* <a href="http://twanvl.nl/blog/haskell/Knight1">part 1: moves</a>

In my previous post I introduced the 'knight moves problem': How many ways are there for a chess knight to reach cell $(i,j)$ in exactly $n$ moves?
The recursive solution from last time is horribly inefficient for larger values of $n$. Today I will show some more efficient solutions.

== Ignoring the order of moves ==

> -- HIDDEN
> module Knight2 where
> 
> import Knight1
> import UnboundedArray
> import Control.Monad

If the knight first makes a move $(-1,2)$ and then a move $(2,1)$ it will end up at $(1,3)$.
If it first moves $(2,1)$ and then $(-1,2)$ it will also end up at $(1,3)$.
So, the order in which the moves happen does not matter for the final position!
We can exploit this fact to make a faster program.
Instead of determining what move to make at each step, we can count how many moves we make of each type and then determine in how many different orders these moves can be performed.

Denote by @n__1@ the number of moves of the first type, @n__123@ the number of moves of type 1, 2 or 3, etc.
So @n__1234 = n__1+n__2+n__3+n__4@, and since there are eight different moves, @n__12345678 = n@.
A count @n__ab@ can be ''split'' into @n__a+n__b@ in several ways, for now we will consider all possibilities:

> split n = [ (i,n-i) | i <- [0..n] ]

So for example, @split 3 = [(0,3),(1,2),(2,1),(3,0)]@.

By repeatedly splitting @n@ we arrive at:

> paths__split n (i,j) = sum $ do
>     let n__12345678 = n
>     (n__1,n__2345678) <- split n__12345678
>     (n__2,n__345678) <- split n__2345678
>     (n__3,n__45678) <- split n__345678
>     (n__4,n__5678) <- split n__45678
>     (n__5,n__678) <- split n__5678
>     (n__6,n__78) <- split n__678
>     (n__7,n__8) <- split n__78
>     let counts = [n__1,n__2,n__3,n__4,n__5,n__6,n__7,n__8]
>     guard $ (i,j) == destination counts
>     return $ multinomial counts

Here we only keep sequences of moves that end up in $(i,j)$, as determined by the @destination@ function:

> destination counts = (sum hs, sum vs)
>     where (hs,vs) = unzip [ (n*δ__i,n*δ__j) | (n,(δ__i,δ__j)) <- zip counts moves ]


Next, we need to know how many different paths can be formed with a particular set of moves.
You might remember <a href="http://en.wikipedia.org/wiki/Binomial_coefficient">binomial coefficients</a> from high school, which give the number of ways to pick $k$ items from a set of size $n$:
<img src="image/knight/multinomial-2-1-1.png" alt="multinomial [2,1,1]" style="float:right;margin-left:1em;border:1px solid #aaa;">

FORMULA: knight/binomial
	\binom{n}{k} = \frac{n!}{k!(n-k)!}

If we take $n$ equal to $m+k$ we get the number of different lists containing exactly $k$ red balls and $m$ green balls. Or put differently, the number of different paths containing $k$ moves of the first type and $m$ moves of the second type. This interpretation of binomial coefficients can be generalized two more than two types, giving ''multinomial coefficients''. These are exactly what we need to determine the number of paths given the counts of each type of move:

> multinomial xs | any (< 0) xs = 0
> multinomial xs = factorial (sum xs)
>                `div` product (map factorial xs)

This multinomial function requires calculating a lot of factorials, to make this as fast as possible they should be stored in an <a href="http://twanvl.nl/blog/haskell/UnboundedArray">'array'</a>:

> factorial :: Int -> Integer
> factorial = unboundedArray $ scanl (*) 1 [1..]

Calculating @paths__split@ only takes $O(n^7)$ integer operations, since each @split@ effectively costs a factor @n@.
While this is better than the previous result, it is still not satisfactory.


== Solving the guard condition ==

The above function uses a "generate and test" approach: Generate all possibilities and test which ones reach the destination. It would be more efficient to generate ''only'' those possibilities.

Algebraic reasoning can help us here. Let's start by expanding the condition in the @guard@ statement:
]> -- BLOCK: haskell-equational-reasoning
]>    (i,j) == destination counts
]> = {- by definition of destination -}
]>    (i,j) == (sum hs, sum vs)
]>      where (hs,vs) = unzip [ (n*δ__i,n*δ__j) | (n,(δ__i,δ__j)) <- zip counts moves ]
]> = {- expand unzip and simplify -}
]>    i == sum (zipWith (*) counts (map fst moves) &&
]>    j == sum (zipWith (*) counts (map snd moves)
]> = {- by definition of moves (see previous post) -}
]>    i == sum (zipWith (*) counts [2,2,-2,-2,1,-1,1,-1] &&
]>    j == sum (zipWith (*) counts [1,-1,1,-1,2,2,-2,-2]
]> = {- expanding the sum and product, remember n__12 = n__1+n__2, etc. -}
]>    i == 2*n__12 - 2*n__34 + n__57 - n__68 &&
]>    j == 2*n__56 - 2*n__78 + n__13 - n__24
]> = {- reordering -}
]>    n__57 - n__68 == i - 2*n__12 + 2*n__34 &&
]>    n__13 - n__24 == j - 2*n__56 + 2*n__78

These are equations we can work with.
Take the equation involving @i@. We know that @n__57 + n__68 = n__5678@, and that @n__57 - n__68 == i - 2*n__12 + 2*n__34@. From these two equations, we can solve for @n__57@ and @n__68@, without needing an expensive @split@:

> -- | find a and b such that a+b == c, a-b == d, a,b >= 0
> solve__pm c d
>    | ok == 0 && a >= 0 && a <= c = return (a,c-a)
>    | otherwise                   = mzero
>  where (a,ok) = (c + d) `divMod` 2

This gives an $O(n^5)$ algorithm:

> paths__pm n (i,j) = sum $ do
>     let n__12345678 = n
>     (n__1234,n__5678) <- split n__12345678
>     (n__12,n__34) <- split n__1234
>     (n__56,n__78) <- split n__5678
>     (n__57,n__68) <- solve__pm n__5678 (i - 2*n__12 + 2*n__34)
>     (n__13,n__24) <- solve__pm n__1234 (j - 2*n__56 + 2*n__78)
>     (n__1,n__2) <- split n__12
>     let n__3 = n__13 - n__1
>     let n__4 = n__24 - n__2
>     (n__5,n__6) <- split n__56
>     let n__7 = n__57 - n__5
>     let n__8 = n__68 - n__6
>     return $ multinomial [n__1,n__2,n__3,n__4,n__5,n__6,n__7,n__8]



== Multinomial laws ==

It turns out that we don't actually need to know @n__1@, @n__2@, etc.
If you think about it, the multinomial coefficient @[n__1,n__2,n__3,n__4,n__5,n__6,n__7,n__8]@ means: "The number of different lists with $n__1$ red balls, $n__2$ of green balls, etc.". To make such a list we can first pick where to put the red balls, then where to put the blue balls, then the green balls and so on.

But we could also first decide where the brightly colored balls (red and green) go and where the dark collored ones (blue) go. Now there are only two types of balls, so this is a binomial coefficient, or in terms of a multinomial, @multinomial [n__rg,n__b]@. Then for the positions with brightly colored balls, we need to determine which ones are which color, which can be done in @multinomial [n__r,n__g]@ ways.  In a picture:

<img src="image/knight/multinomial-law1.png" alt="multinomial [2,1,1] = multinomial [3,1] * multinomial [2,1]" style="margin-left:2em;border:1px solid #aaa;">

This same arguments also holds when there are eight types of balls (or moves), so

]> multinomial [n__1,n__2,n__3,n__4,n__5,n__6,n__7,n__8]
]>  == multinomial [n__12,n__34,n__56,n__78]
]>   * multinomial [n__1,n__2] * multinomial[n__3,n__4]
]>   * multinomial [n__5,n__6] * multinomial[n__7,n__8]

If you plug this into the @paths__pm@ function, you might notice that the last part of the function is calculating the product of two independent things. One part is about @n__1..n__4@ and the other about @n__5..n__8@.
Now remember that the function @paths@ takes the sum of all possibilities, and that products distributes over sums. This means that the two loops for @n__1234@ and @n__5678@ can be performed independently, giving us an $O(n^4)$ algorithm:

> paths__O4 n (i,j) = sum $ do
>     let n__12345678 = n
>     (n__1234,n__5678) <- split n__12345678
>     (n__12,n__34) <- split n__1234
>     (n__56,n__78) <- split n__5678
>     (n__57,n__68) <- solve__pm n__5678 (i - 2*n__12 + 2*n__34)
>     (n__13,n__24) <- solve__pm n__1234 (j - 2*n__56 + 2*n__78)
>     let result__1234 = sum $ do
>          (n__1,n__2) <- split n__12
>          let n__3 = n__13 - n__1
>          let n__4 = n__24 - n__2
>          return $ multinomial [n__1,n__2] * multinomial[n__3,n__4]
>     let result__5678 = sum $ do
>          (n__5,n__6) <- split n__56
>          let n__7 = n__57 - n__5
>          let n__8 = n__68 - n__6
>          return $ multinomial [n__5,n__6] * multinomial[n__7,n__8]
>     return $ multinomial [n__12,n__34,n__56,n__78] * result__1234 * result__5678

Here both of the @result@ parts are of the form

]> sum [ multinomial [a,b] * multinomial[x-a,y-b] | (a,b) <- split n ]

which just so happens to be equivalent to just @multinomial [x,y]@ (a proof of this statement is left as an exercise, i.e. I am too lazy to write it out).
This equation immediately leads to a (much simpler) $O(n^3)$ algorithm:

> paths__O3 n (i,j) = sum $ do
>     let n__12345678 = n
>     (n__1234,n__5678) <- split n__12345678
>     (n__12,n__34) <- split n__1234
>     (n__56,n__78) <- split n__5678
>     (n__57,n__68) <- solve__pm n__5678 (i - 2*n__12 + 2*n__34)
>     (n__13,n__24) <- solve__pm n__1234 (j - 2*n__56 + 2*n__78)
>     return $ multinomial [n__12,n__34,n__56,n__78]
>            * multinomial [n__57,n__68]
>            * multinomial [n__13,n__24]

== Verifying the results ==

After all this manipulation it is a good idea to check whether the program still does the right thing.
We can either manually compare the path matrices:

> check paths = and [ pathMatrix paths__rec n == pathMatrix paths n | n <- [0..3] ]

Or use QuickCheck or SmallCheck:

]> Knight2> smallCheck 5 (\(N n) ij -> paths__O3 n ij == paths__rec n ij)
] ...
] Depth 5:
]   Completed 726 test(s) without failure.


Finally, to contrast with the first part of this series, here is the time it takes to calculate the number of paths in $100$ steps:

]> Knight2> paths__O3 100 (4,4)
] 2422219241769802380469882122062019059350760968380804461263234408581143863208781993964800
] (4.75 secs, 270708940 bytes)

The recursive algorithm would need in the order of $10^77$ years to arrive at this answer.

Still, @paths__O3@ is not the fastest possible algorithm.
Next time I will look at a completely different approach, but further improvements to the solution in this post are possible as well.
As an exercise for the reader, you should try transforming @paths__O3@ into an $O(n^2)$ solution. Hint: there are more sums-of-products of independent values.

