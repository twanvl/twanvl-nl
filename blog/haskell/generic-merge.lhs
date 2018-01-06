title: A generic merge function
subtitle: Generalizing union, intersection and set difference
date: 2008-08-15
tags: haskell
sourcelink: This post is literate Haskell, click here to download the source code.

> -- IGNORE
> {-# LANGUAGE NoMonomorphismRestriction #-}

When working with sorted lists you often come to the point where you want to combine two or more of them.
This ''merge'' procedure forms the heart of <a href="http://en.wikipedia.org/wiki/Merge_sort">merge sort</a>
it works something like:

]> -- BLOCK: haskell-equation
]> merge [1,3,4,5] [2,3,4] = [1,2,3,3,4,4,5]


This @merge@ function is not in the Haskell standard library, and even if there were, it might not be very useful.

The problem is that when you need @merge@ you often need a slight variation.
For example, you might want to remove duplicates,
]> merge_union [1,3,4,5] [2,3,4] = [1,2,3,4,5]
Or find the elements common to both lists,
]> merge_intersection [1,3,4,5] [2,3,4] = [3,4]
Or you want the difference, the symmetric difference, or...


<br>The solution for all these problems is to make a more general @merge@ function.
To do that we take a note from the most generic function over a single list, @foldr@.
The generic merge function is also a right fold, but over two lists.
Behold the type signature:

> mergeByR :: (a -> b -> Ordering)  -- ^ cmp: Comparison function
>          -> (a -> b -> c -> c)    -- ^ f__xy: Combine when a and b are equal
>          -> (a -> c -> c)         -- ^ f__x:  Combine when a is less
>          -> (b -> c -> c)         -- ^ f__y:  Combine when b is less
>          -> c                     -- ^ z:   Base case
>          -> [a] -> [b] -> c       -- ^ Argument lists and result list

Don't be scared by the size. The reason there are a lot of arguments is that for each case we use a different combining function:
If the smallest element comes from the first list we use @f__x@, if it comes from the second list we use @f__y@, and when the two elements are equal, we combine them both with @f__xy@.
As in @foldr@ these calls to @f__x/f__y/f__xy@ are then chained like @f__x x__1 (f__x x__2 (.. z))@.

The lists from the example above can be aligned as follows:

]> -- BLOCK: haskell-equation
]> xs               =  [1,      3,   4,   5 ]
]> ys               =  [    2,  3,   4      ]
]> function to use  =  [f__x, f__y, f__xy, f__xy, f__x]
]> mergeByR ....    =  f__x 1 . f__y 2 . f__xy 3 3 . f__xy 4 4 . f__x 5 $ z

The function implementation is straightforward:

> mergeByR cmp f__xy f__x f__y z = go
>     where go []     ys     = foldr f__y z ys
>           go xs     []     = foldr f__x z xs
>           go (x:xs) (y:ys) = case cmp x y of
>               LT -> f__x  x   (go xs (y:ys))
>               EQ -> f__xy x y (go xs ys)
>               GT -> f__y    y (go (x:xs) ys)



<br>Now, let's look at some uses of this function.
First of all, the usual merge sort @merge@ function:

> mergeBy cmp = mergeByR cmp (\a b c -> a:b:c) (:) (:) []
> merge = mergeBy compare

Instead of adding both @a@ and @b@ to the resulting list when they are equal, we can instead add only one of them, or even the result of some function on them.
This gives the set @union@ operation:

> unionByWith cmp f = mergeByR cmp (\a b c -> f a b:c) (:) (:) []
> unionWith = unionByWith compare

If we ignore elements that occur in only one of the lists by setting @f__x@ and @f__y@ to @const id@, we get the intersection instead:

> intersectionByWith cmp f = mergeByR cmp (\a b c -> f a b:c) (const id) (const id) []
> intersectionWith = intersectionByWith compare


<br>With these merge functions, implementing merge sort becomes simple.
All that is left to do is split a list in two, and recursively sort and merge.

> split :: [a] -> ([a],[a])
> split (x:y:zs) = let (xs,ys) = split zs in (x:xs,y:ys)
> split xs       = (xs,[])
> 
> sort []  = []
> sort [x] = [x]
> sort xs  = let (ys,zs) = split xs in merge (sort ys) (sort zs)

If we replace @merge@ by @unionWith@ we instead get a sort that combines duplicate elements.



<br>Besides set operations, @mergeByR@ can also be (ab)used for other things, such as

> zipWith = intersectionByWith (const $ const EQ)

Or a variant of @zipWith@, that keeps the tail of the longer list:

> zipWith' = unionByWith (const $ const EQ)

We can even implement concatenation:

> (++) = mergeByR (const $ const LT) undefined (:) (:) []
