title: Search trees without sorting
subtitle: Efficient searches with the help of a semilattice.
date: 2012-02-07 23:56CET
tag: haskell
sourcelink: This post is literate Haskell, click here to download the source code.

> -- IGNORE
> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
> module SemilatticeSearchTree where
>
> import Control.Monad (mplus)

Binary search trees are used quite often for storing or finding values.
Like a binary search, they essentially work by sorting the items.

In this post I will describe a search tree that does not require that the items be sorted. Hence, the tree can support some interesting queries. The queries will always be correct, but they will only be fast in some cases.

-- Bounds --

Usually, to make searching fast, each branch in a search tree stores information that helps to decide whether to go left or right. But if we want to be able to construct a tree for any possible type of query, then that is not always possible.
Instead, we can still aim to eliminate large parts of the search space, by storing bounds.

Suppose we have a tree that stores integers, and we want to find the first item in the tree that is greater or equal to some query integer. In each branch of the tree, we could store the maximum of all values in that subtree. Call it the upper bound of the subtree. If this upper bound is less than the query, then we can eliminate the entire subtree from consideration.

Now let's generalize that. The maximum value is an example of a <a href="http://en.wikipedia.org/wiki/Semilattice">semilattice</a>. That is just a fancy way of saying that for a pair of values we can get some kind of bound. As a typeclass it looks like

> class Semilattice a where
>     meet :: a -> a -> a
>     -- Laws: meet is associative, commutative and idempotent:
>     --         meet a (meet b c) = meet (meet a b) c
>     --         meet a b = meet b a
>     --         meet a a = a

The queries we perform on the tree should of course work together with the bounds.
That means that if a bound for a branch in the tree doesn't satisfy the query, then none of the values in the subtree do. In haskell terms:

> class Semilattice a => Satisfy q a | q -> a where
>     satisfy :: q -> a -> Bool
>     -- Law: satisfy q a || satisfy q b  ==>  satisfy q (meet a b)

Note that a semilattice always gives a partial order, and hence a satisfy function by
]> satisfy q a = meet q a == a
because
]> --BLOCK: haskell-equational-reasoning
]>      satisfy q a || satisfy q b
]> <=>  meet q a == a || meet q b == b
]> ==>  meet (meet q a) b == meet a b || meet a (meet q b) == meet a b
]> <=>  meet q (meet a b) == meet a b || meet q (meet a b) == meet a b
]> <=>  meet q (meet a b) == meet a b
]> <=>  satisfy q (meet a b)
However, I keep the distinction between the query and value type for more flexibility and for more descriptive types.

-- Implementation --

Given the @Satisfy@ and @Semilattice@ typeclasses, the search tree datastructure is straight forward.
A search tree can be empty, a single value, or a branch. In each branch we store the bound of that subtree.

> data SearchTree a
>     = Empty
>     | Leaf !a
>     | Branch !a (SearchTree a) (SearchTree a)
>     deriving (Show)
> 
> bound :: SearchTree a -> a
> bound (Leaf a) = a
> bound (Branch a _ _) = a
> bound Empty = error "bound Empty"

If we have a @SearchTree@, then we can find the first element that satisfies a query, simply by searching both sides of each branch. The trick to making the search faster is to only continue as long as the bound satisfies the query:

> -- Find the first element in the tree that satisfies the query
> findFirst :: Satisfy q a => q -> SearchTree a -> Maybe a
> findFirst q (Leaf a)       | satisfy q a = Just a
> findFirst q (Branch a x y) | satisfy q a = findFirst q x `mplus` findFirst q y
> findFirst _ _ = Nothing

Completely analogously, we can find the last satisfied item instead:

> -- Find the last element in the tree that satisfies the query
> findLast :: Satisfy q a => q -> SearchTree a -> Maybe a
> findLast q (Leaf a)       | satisfy q a = Just a
> findLast q (Branch a x y) | satisfy q a = findLast q y `mplus` findLast q x
> findLast _ _ = Nothing

Or we can even generalize this search to any @Monoid@, where the above are for the <a href="http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html#t:First">@First@</a> and <a href="http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html#t:Last">@Last@</a> monoids respectively. I will this leave as an exercise for the reader.

-- Constructing --

The basis of each tree are branches. We will always construct branches with a smart constructor that calculates the bound as the meet of the bounds of its two arguments. That way, the stored bound is always correct.

> mkBranch :: Semilattice a => SearchTree a -> SearchTree a -> SearchTree a
> mkBranch Empty y = y
> mkBranch x Empty = x
> mkBranch x y = Branch (bound x `meet` bound y) x y

A search will always take time at least linear in the <em>depth</em> of the tree. So, for fast searches we need a balanced tree, where each subtree has roughly the same size. Here is arguably the most tricky part of the code, which converts a list to a balanced search tree.

> -- /O(n*log n)/
> -- Convert a list to a balanced search tree
> fromList :: Semilattice a => [a] -> SearchTree a
> fromList []  = Empty
> fromList [x] = Leaf x
> fromList xs  = mkBranch (fromList ys) (fromList zs)
>   where
>     (ys,zs) = splitAt (length xs `div` 2) xs

And that's it.
I use this data structure for finding rectangles (more about that in a future post), and there I only needed to build the search structure once, and use it multiple times. So, in this post I am not going to talk about updates at all. If you wanted to do updates efficiently, then you would need to worry about updating bounds, rebalancing etc.

-- Example uses --

Here is an example of the search tree in action.
The query will be to find a value @(>= q)@ for a given @q@. The bounds will be maximum values.

> newtype Max a = Max { getMax :: a } deriving (Show)
> instance Ord a => Semilattice (Max a) where
>     meet (Max a) (Max b) = Max (max a b)
> 
> newtype Ge a = Ge a deriving (Show)
> instance Ord a => Satisfy (Ge a) (Max a) where
>     satisfy (Ge q) = (>= q) . getMax

> -- IGNORE
> -- analogous to the above
> newtype Gt a = Gt a deriving (Show)
> instance Ord a => Satisfy (Gt a) (Max a) where
>     satisfy (Gt q) = (> q) . getMax
> 
> newtype Min a = Min { getMin :: a } deriving (Show)
> instance Ord a => Semilattice (Min a) where
>     meet (Min a) (Min b) = Min (min a b)
> 
> newtype Le a = Le a deriving (Show)
> instance Ord a => Satisfy (Le a) (Min a) where
>     satisfy (Le q) = (<= q) . getMin
> 
> newtype Lt a = Lt a deriving (Show)
> instance Ord a => Satisfy (Lt a) (Min a) where
>     satisfy (Lt q) = (<  q) . getMin


First, check the @satisfy@ law:
]> --BLOCK: haskell-equational-reasoning
]>      satisfy (Ge q) (Max a) || satisfy (Ge q) (Max b)
]> <=>  a >= q || b >= q
]> <=>  if a >= b then a >= q else b >= q
]> <=>  (if a >= b then a else b) >= q
]> <=>  max a b >= q
]> <=>  satisfy (Ge q) (Max (max a b))
]> <=>  satisfy (Ge q) (meet (Max a) (Max b))

<div style="width:240px;" class="float-right"><img src="image/searchtree/tree1.png" alt=""><br>The search tree corresponding to @fromList (map Max [1..5])@. Circles are @Leaf@s and squares are @Branch@es.</div>

So indeed, @satisfy q a || satisfy q b  ==>  satisfy q (meet a b)@.
And this bound is in fact tight, so also the other way around @satisfy q (meet a b)  ==>  satisfy q a || satisfy q b@. This will become important later.

Now here are some example queries:

]> λ> findFirst (Ge 3) (fromList $ map Max [1,2,3,4,5])
]> Just (Max 3)
]> λ> findFirst (Ge 3) (fromList $ map Max [2,4,6])
]> Just (Max 4)
]> λ> findFirst (Ge 3) (fromList $ map Max [6,4,2])
]> Just (Max 6)
]> λ> findFirst (Ge 7) (fromList $ map Max [2,4,6])
]> Nothing

Semilattices and queries can easily be combined into tuples. For a tree of pairs, and queries of pairs, you could use.

> -- IGNORE
> -- Note: this instance requires UndecidableInstances when we use a FunDep for Satisfy

> instance (Semilattice a, Semilattice b) => Semilattice (a,b) where
>     meet (a,b) (c,d) = (meet a c, meet b d)
> instance (Satisfy a b, Satisfy c d) => Satisfy (a,c) (b,d) where
>     satisfy (a,c) (b,d) = satisfy a b && satisfy c d

Now we can not only questions like "What is the first/last/smallest element that is greater than some given query?". But also "What is the first/last/smallest element greater than a given query that also satisfies some other property?".

-- When is it efficient? --

It's nice that we now have a search tree that always gives correct answers.
But is it also efficient?

As hinted in the introduction, that is not always the case.
First of all, @meet@ could give a really bad bound. For example, if @meet a b = Bottom@ for all @a /= b@, and Bottom satisfies everything, then we really can do no better than a brute force search.

On the other hand, suppose that @meet@ gives 'perfect' information, like the @Ge@ example above,
]> --BLOCK: haskell-equational-reasoning
]> satisfy q (meet a b)  ==>  satisfy q a || satisfy q b
That is equivalent to saying that
]> --BLOCK: haskell-equational-reasoning
]> not (satisfy q a) && not (satisfy q b) ==> not (satisfy q (meet a b))

Then for any Branch, we only have to search either the left or the right subtree.
Because, if a subtree doesn't contain the value, we know can see so from the bound.
For a balanced tree, that means the search takes $O(log n)$ time.



Another efficient case is when the items are sorted.
By that I mean that, if an item satisfies the query, then all items after it also satisfy that query.
We actually need something slightly more restrictive: namely that if a query is satisfied for the meet of some items, then all items after them also satisfy the query.
In terms of code:
]> --BLOCK: haskell-equational-reasoning
]> let st = fromList (xs__1 ++ xs__2 ++ xs__3)
]> satisfy q (meet xs__2)  ==>  all (satisfy q) xs__3

Now suppose that we are searching a tree of the form @st = mkBranch a b@ with @findFirst q@. Then there are three cases:

<ol><li>@not (satisfy q (bound st))@.
<li>@not (satisfy q (bound a))@.
<li>@satisfy q (bound a)@.</ol>

In the first case the search fails, and we are done.
In the second case, we only have to search @b@, which by induction can be done efficiently.
The third case is not so clear.
In fact, there are two sub cases:

<ul style="list-style:none;padding:0 1em;"><li>3a. findFirst q a = Just someResult
<li>3b. findFirst q b = Nothing</ul>

In case 3a we found something in the left branch. Since we are only interested in the first result, that means we are done.
In case 3b, we get to use the fact that the items are sorted. Since we have @satisfy q (bound a)@, that means that all items in @b@ will satisfy the query. So when searching @b@, in all cases we take the left branch.

Overall, the search time will be at most twice the depth of the tree, which is $O(log n)$.


The really cool thing is that we can combine the two conditions. If satisfy can be written as
]> satisfy q a == satisfy__1 q a && satisfy__2 q a
where @satisfy__1@ has exact bounds, and the tree is sorted for @satisfy__2@, then queries still take $O(log n)$ time.

-- Closing example --

Finally, here is an example that makes use of efficient searching with the two conditions.
I make use of the @Semilattice@ and @Satisfy@ instances for pairs which I defined above.

> treeOfPresidents :: SearchTree (Max Int, Max String)
> treeOfPresidents
>    = fromList [ (Max year, Max name) | (year,name) <- usPresidents ]
>  where
>    usPresidents =
>      [(1789,"George Washington")
>      ,(1797,"John Adams")
>      ,(1801,"Thomas Jefferson")
>      -- etc

> -- IGNORE
>      ,(1809,"James Madison")
>      ,(1817,"James Monroe")
>      ,(1825,"John Quincy Adams")
>      ,(1829,"Andrew Jackson")
>      ,(1837,"Martin Van Buren")
>      ,(1841,"William Henry Harrison")
>      ,(1841,"John Tyler")
>      ,(1845,"James K. Polk")
>      ,(1849,"Zachary Taylor")
>      ,(1850,"Millard Fillmore")
>      ,(1853,"Franklin Pierce")
>      ,(1857,"James Buchanan")
>      ,(1861,"Abraham Lincoln")
>      ,(1865,"Andrew Johnson")
>      ,(1869,"Ulysses S. Grant")
>      ,(1877,"Rutherford B. Hayes")
>      ,(1881,"James A. Garfield")
>      ,(1881,"Chester A. Arthur")
>      ,(1885,"Grover Cleveland")
>      ,(1889,"Benjamin Harrison")
>      ,(1893,"Grover Cleveland")
>      ,(1897,"William McKinley")
>      ,(1901,"Theodore Roosevelt")
>      ,(1909,"William Howard Taft")
>      ,(1913,"Woodrow Wilson")
>      ,(1921,"Warren G. Harding")
>      ,(1923,"Calvin Coolidge")
>      ,(1929,"Herbert Hoover")
>      ,(1933,"Franklin D. Roosevelt")
>      ,(1945,"Harry S Truman")
>      ,(1953,"Dwight D. Eisenhower")
>      ,(1961,"John F. Kennedy")
>      ,(1963,"Lyndon B. Johnson")
>      ,(1969,"Richard Nixon")
>      ,(1974,"Gerald Ford")
>      ,(1977,"Jimmy Carter")
>      ,(1981,"Ronald Reagan")
>      ,(1989,"George H. W. Bush")
>      ,(1993,"Bill Clinton")
>      ,(2001,"George W. Bush")
>      ,(2009,"Barack Obama")]

The tree is ordered by year of election, and the @Max@ semilattice gives tight bounds for names. So we can efficiently search for the first US presidents elected after 1850 who's name comes starts with a letter after "P":

]> λ> findFirst (Ge 1850,Ge "P") treeOfPresidents
]> Just (Max 1869,Max "Ulysses S. Grant")

And with the following query type we can search on just one of the elements of the tuple.
Note that we need the type parameter in @Any@ because of the functional dependency in the @Satisfy@ class.

> data Any s = Any
> instance Semilattice s => Satisfy (Any s) s where
>     satisfy _ _ = True

]> λ> findFirst (Ge 1911,Any) treeOfPresidents
]> Just (Max 1913,Max "Woodrow Wilson")


