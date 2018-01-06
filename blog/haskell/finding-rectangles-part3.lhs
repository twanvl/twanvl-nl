title: Finding rectangles, part 3: divide and conquer
subtitle: A faster algorithm for finding border rectangles in images.
date: 2012-02-11 23:13CET
tag: haskell, rectangles
sourcelink: This post (including diagrams) is literate Haskell, click here to download the source code.

> -- HIDDEN
> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
> import Control.Monad
> import Control.Monad.Trans
> import Data.Ord
> import Data.List
> import Data.Monoid
> --import Graphics.GridDiagrams
> import Test.QuickCheck
> import SemilatticeSearchTree

In <a href="blog/haskell/finding-rectangles-part2">part 2 of this series</a>, I looked at finding axis aligned rectangles in binary images. 
I left you hanging with a hint of a more efficient algorithm than the $O(n^3)$ one from that post.
Formally, the problem we were trying to solve was:

<blockquote style="font-style:italic;">
Given a binary image, find the largest axis aligned rectangle with a 1 pixel wide border that consists entirely of foreground pixels.
</blockquote>

Here is the same example as last time,
 <br><img src="image/find-rect/rects2-best.png" style="margin-left:2em;">,<br>
where white pixels are the background and blue is the foreground.
The rectangle with the largest area is indicated in red.


-- A rectangle is two brackets --

The idea behind the more efficient algorithm is simple.
Draw a vertical line $x=x_{mid}$ through the middle of the image,
 <br><img src="image/find-rect/rects2-mid.png" style="margin-left:2em;">.<br>
If the largest rectangle in an image is large enough, then it will intersect this line. The one in the example above certainly does.
So, the idea is to only look at rectangles that intersect $x=x_{mid}$. We will worry about other cases later.

Each rectangle that intersects the vertical line consists of of a ''left bracket'' and a ''right bracket'', just look at this ASCII art: <tt>[]</tt>, or at these images:
 <br>
 <img src="image/find-rect/rects2-lbracket.png" style="margin-left:2em;vertical-align:middle;"> and 
 <img src="image/find-rect/rects2-rbracket.png" style="vertical-align:middle;">.

To find all rectangles intersecting $x=x_{mid}$, we need to find these left and right brackets, and combine them.
Note that the middle column is included in both the left and right bracket, because that makes it easier to handle rectangles and bracked of width=1.

Let's focus on finding the right brackets first. 
For each pair of $y$-coordinates and height, there is at most one largest right bracket. We don't need to consider the smaller ones.
So, let's define a function that finds the width of the largest right bracket for all $y$-coordinates and heights. The function takes as input just the right part of the image, and it will return the result in list of lists:

> rightBracketWidths :: Image -> [[Maybe Int]]

Here is a slow 'specification-style' implementation

> rightBracketWidths__slow im
>   = [ [ findLast (containsBracket im y h) [1..imWidth im]
>       | h <- [1..imHeight im-y]
>       ]
>     | y <- [0..imHeight im-1]
>     ]
>   where findLast pred = find pred . reverse

How do we even check that a right bracket is in an image?
For that we can look at <a href="blog/haskell/finding-rectangles#h2-finding-lines">right and bottom endpoints</a>:

> -- BLOCK: haskell-pseudo-code
> -- pseudo code
> containsBracket im y h w
>     = r !! y       !! 0     >= w   -- top border
>    && r !! (y+h-1) !! 0     >= w   -- bottom border
>    && b !! y       !! (w-1) >= y+h -- right border

> -- IGNORE
> -- horribly inefficient implementation
>   where
>     xx = scanRightward (\_ x -> x + 1) (-1) im :: [[Int]]
>     yy = scanDownward  (\_ y -> y + 1) (-1) im :: [[Int]]
>     r  = scanLeftward  (\(a,x) r -> if a then r else x) (imWidth im)  (zip2d im xx)
>     b  = scanUpward    (\(a,y) b -> if a then b else y) (imHeight im) (zip2d im yy)

Here I used two dimensional indexing. So @r!!y!!0@ stands for the right endpoint for column @0@ of row @y@; or in other words, the number of foreground pixels at the start of that row.

This image illustrates the right endpoints of the top and bottom border in green, and the bottom endpoint of the right border in red. The bracket (in glorious pink) has to fit between these indicated endpoints.
 <br>
 <img src="image/find-rect/rects2-rbracket-conditions.png" style="margin-left:2em;">.

The @rightBracketWidths__slow@ function is, as the name suggests, slow.
It does a linear search over the possible widths. With that it would take $O(m^2*n)$ to find all widths for an $m$ by $n$ image. That is no better than the complexity of the algorithm from last time.


-- Faster searching --

In <a href="blog/haskell/SemilatticeSearchTree">my previous blog post</a>, I introduced a @SearchTree@ type that answers just the type @findLast@ query that we need. In fact, this rectangle problem was why I made that @SearchTree@ data structure in the first place.

There are three conditions in @containsBracket@.
We will handle the one for the top border, @r!!y!!0 >= w@ by building a separate search tree for each @y@. This search tree then only the widths @w <- [1..r!!y!!0]@.

That leaves the conditions on the bottom and right borders.
Since we fixed @y@, we can directly write these conditions in terms of a @SearchTree@ query:
For the bottom border we need @satisfy (Ge (y+h)) (b!!y!!(w-1))@, and for the right border @satisfy (Le (r!!(y+h-1)!!0)) w@.
As you can hopefully see, these are exactly the same as the conditions in the @containsBracket@ function above.

We can combine the two conditions into a pair, to give @(Ge (y+h), Le (r!!(y+h-1)!!0))@.
While, to build the search tree, we need to pair @b!!y!!(w-1)@ with @w@. That is just a matter of zipping two lists together:

> bracketsAtY :: Int -> [Int] -> [Int] -> [Maybe Int]
> bracketsAtY y bs__y rs@(r__y:_)
>     = [ fmap (\(Max b__yw1, Min w) -> w)
>              (findLast (Ge (y+h),Le r__yh1) searchTree)
>       | (h,r__yh1) <- zip [1..] rs
>       ]
>   where
>     searchTree = fromList [ (Max b__yw1, Min w) | (b__yw1,w) <- zip bs__y [1..r__y] ]
> -- notation:
> --    bs__y  = [b!!y!!0, b!!y!!1, ..]
> --    rs     = [r!!y!!0, r!!(y+1)!!0, ..]
> --    b__yw1 = b!!y!!(w-1)
> --    r__y   = r!!y!!0
> --    r__yh1 = r!!(y+h-1)!!0

We need to call @bracketsAtY@ for each @y@, together with the right row of bottom endpoints, and right endpoints:

> rightBracketWidths a = zipWith3 bracketsAtY [0..] b (tails (map head r))
>   where
>     -- as in !!!<a href="blog/haskell/finding-rectangles">the previous posts</a>!!!
>     x = scanRightward (\_ x -> x + 1) (-1) a :: [[Int]]
>     y = scanDownward  (\_ y -> y + 1) (-1) a :: [[Int]]
>     r = scanLeftward  (\(a,x) r -> if a then r else x) (imWidth a) (zip2d a x)
>     b = scanUpward    (\(a,y) b -> if a then b else y) (imHeight a) (zip2d a y)

QuickCheck will confirm that this function is the same as the slow version above:

> prop_rightBracketWidths = forAll genImage $ \im ->
>     rightBracketWidths im == rightBracketWidths__slow im

]> λ> quickCheck prop_rightBracketWidths
]> +++ OK, passed 100 tests.

With the efficient search trees, @bracketsAtY@ takes $O((m+n)*log n)$ time, and @rightBracketWidths@ takes $O(m*(m+n)*log n)$ time for an $m$ by $n$ image. For large images this is much faster than the $O(m^2*n)$ linear search.


-- From brackets to rectangles --

If we have a way of finding right brackets, we can easily reuse that function for left brackets, by just flipping the image.

> leftBracketWidths :: Image -> [[Maybe Int]]
> leftBracketWidths = rightBracketWidths . flipHorziontal
>   where flipHorziontal = map reverse

Once we have the left and right brackets, we can combine them into rectangles.
Suppose that a left bracket has width @lw@, and the right bracket @rw@.
Then the width of the rectangle they form is @lw+rw-1@, since both include the middle line.

> combineBrackets :: Int -> [[Maybe Int]] -> [[Maybe Int]] -> [Rect]
> combineBrackets x__mid lbrackets rbrackets
>     = [ Rect (x__mid-lw+1) y (lw+rw-1) h
>       | (y,lws,rws) <- zip3 [0..] lbrackets rbrackets
>       , (h,Just lw,Just rw) <- zip3 [1..] lws rws
>       ]

And finding (a superset of) all the maximal rectangles intersecting the vertical line $x=x_{mid}$ can be done by cutting the image on that line, finding brackets, and combining them.

> rectsIntersecting :: Int -> Image -> [Rect]
> rectsIntersecting x__mid im = combineBrackets x__mid brackets__left brackets__right
>   where
>     im__left  = map (take (x__mid+1)) im
>     im__right = map (drop x__mid) im
>     brackets__left  = leftBracketWidths im__left
>     brackets__right = rightBracketWidths im__right

-- Divide and conquer --

We left out one important case: what if the largest rectangle does not intersect the mid-line?
For that purpose man has invented recursion:
First look for rectangles intersecting the middle, and then look for rectangles not intersecting the middle. For that we need to look in the left and right halves.

To make this asymptotically fast, we have to ensure that both the width and height decrease. 
Since the time complexity of @rectsIntersecting@ includes a $log n$ term, it is faster for wide images. So, if the image is tall, we just transpose it to make it wide instead.

The recursion pattern of vertical and horizontal and middle lines will in the end look something like this:
 <br>
 <img src="image/find-rect/rects2-recurse.png" style="margin-left:2em;">,
 <br>
 with the first level in yellow, the second in green, then magenta and red.
 So in the first level we find all rectangles intersecting the yellow line.
 Then in the second level all rectangles intersecting a green line, and so on.

Here is the code:

> allRectsRecurse :: Image -> [Rect]
> allRectsRecurse im
>     -- empty image ==> no rectangles
>     | imWidth im == 0 || imHeight im == 0
>         = []
>     -- height > width? ==> transpose
>     | imHeight im > imWidth im
>         = map transposeRect . allRectsRecurse . transpose $ im
>     -- find and recruse
>     | otherwise
>         = rectsIntersecting x__mid im -- find
>        ++ findRectsRecurse im__left  -- recurse left
>        ++ map (moveRect (x__mid+1) 0) (allRectsRecurse im__right) -- recurse right
>   where
>     x__mid = imWidth im `div` 2
>     im__left  = map (take x__mid) im -- *excluding* the middle line
>     im__right = map (drop (x__mid+1)) im

where

> transposeRect :: Rect -> Rect
> transposeRect (Rect x y w h) = Rect y x h w
> 
> moveRect :: Int -> Int -> Rect -> Rect
> moveRect dx dy (Rect x y w h) = Rect (x+dx) (y+dy) w h

Since the image is roughly halved in each recursion step, the recursio will have depth $O(log n)$ for an $n$ by $n$ image. At each level, the @rectsIntersecting@ calls will take $O(n^2*log n)$ time, for a total of $O(n^2*(log n)^2)$. This is significantly faster than the $O(n^3)$ from the previous post.

For the complexity theorists: it is possible to do slightly better by using a <a href="http://en.wikipedia.org/wiki/Disjoint-set_data_structure">disjoint-set (union-find) data structure</a> instead of a search tree for finding brackets. I believe that would bring the runtime down to $O(n^2*log n*α(n))$ where α is the inverse Ackermann function. Unfortunately such a data structure requires mutation, the correctness proofs are much harder, and the gain is quite small.

Let me end by checking that the set of maximal rectangles we find are the same as those found by the specification from the previous post. Then by extension the largest rectangle found will also be the same.

> prop_maximalRects = forAll genImage $ \im ->
>       (sort . onlyTrulyMaximalRects . allRects) im
>    == (sort . onlyTrulyMaximalRects . allRectsRecurse) im

]> λ> quickCheck prop_maximalRects
]> +++ OK, passed 100 tests.


> -- HIDDEN
> -----------------------------------------------------------------------------
> -- Quickcheck properties
> -----------------------------------------------------------------------------
> 
> prop_maximalRectsNoRecurse im =
>       (sort . onlyTrulyMaximalRects . allRects) im
>    == (sort . onlyTrulyMaximalRects . rectsIntersecting (imWidth im `div` 2)) im
> 
> -----------------------------------------------------------------------------
> -- Images and stuff
> -----------------------------------------------------------------------------
> 
> -- An image is a 2D list of booleans, True is the foreground
> type Image = [[Bool]]
> 
> -- An axis-aligned rectangle
> data Rect = Rect { left, top, width, height :: Int }
>     deriving (Eq,Ord,Show)

> -- HIDDEN
> -- The size of an image
> imWidth, imHeight :: Image -> Int
> imHeight = length
> imWidth (x:_) = length x
> imWidth []    = 0
> 
> -- The area and perimeter of a rectangle
> area, perimeter :: Rect -> Int
> area rect = width rect * height rect
> perimeter rect = 2 * width rect + 2 * height rect
> 
> -----------------------------------------------------------------------------
> -- Specification from last time
> -----------------------------------------------------------------------------
> 
> contains :: Image -> Rect -> Bool
> contains im rect = isBorder (cropRect im rect)
> 
> -- crop an image to the pixels inside a given rectangle
> cropRect :: Image -> Rect -> Image
> cropRect im (Rect x y w h) = map cols (rows im)
>   where
>     rows = take h . drop y . (++repeat [])
>     cols = take w . drop x . (++repeat False)
> 
> -- is the border of an image foreground?
> isBorder :: Image -> Bool
> isBorder im = firstAndLast and im && all (firstAndLast id) im
>   where
>     firstAndLast prop [] = True
>     firstAndLast prop xs = prop (head xs) && prop (last xs)
> 
> largestRect__spec :: Image -> Rect
> largestRect__spec = maximalRectBy area . allRects
> 
> allRects :: Image -> [Rect]
> allRects im = filter (im `contains`) rects
>   where
>     rects = [Rect x y w h | x <- [0..iw], y <- [0..ih]
>                           , w <- [1..iw-x], h <- [1..ih-y]]
>     iw = imWidth im
>     ih = imHeight im
> 
> -- Return the rectangle with maximum f,
> --  using lexicographical ordering to break ties
> --  return noRect if there are no rectangles in the input list.
> maximalRectBy :: Ord a => (Rect -> a) -> [Rect] -> Rect
> maximalRectBy f = maximumBy (comparing f `mappend` compare) . (noRect:)
>   where noRect = Rect 0 0 0 0
> 
> -----------------------------------------------------------------------------
> -- Other utility functions, see finding-rectangles.lhs
> -----------------------------------------------------------------------------
> 
> onlyTrulyMaximalRects :: [Rect] -> [Rect]
> onlyTrulyMaximalRects xs = filter (\x -> not $ any (\y -> y /= x && y `rectContains` x) xs) xs
> 
> rectContains :: Rect -> Rect -> Bool
> rectContains (Rect x y w h) (Rect x' y' w' h') = x <= x' && y <= y' && x+w >= x'+w' && y+h >= y'+h'
> 
> genImage :: Gen Image
> genImage = sized $ \n ->
>   do w <- choose (0,n)
>      h <- choose (0,n)
>      vectorOf h $ vectorOf w arbitrary
> 
> instance Arbitrary Rect where
>     arbitrary = liftM4 Rect arbitrary arbitrary (liftM abs arbitrary) (liftM abs arbitrary)
> 
> -- HIDDEN
> -----------------------------------------------------------------------------
> -- Zips and scans, see finding-rectangles.lhs
> -----------------------------------------------------------------------------
> 
> zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
> zip2d = zipWith zip
> 
> zip2d3 :: [[a]] -> [[b]] -> [[c]] -> [[(a,b,c)]]
> zip2d3 = zipWith3 zip3
> 
> zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
> zipWith2d = zipWith . zipWith
> 
> zipWith2d4 :: (a -> b -> c -> d -> e) -> [[a]] -> [[b]] -> [[c]] -> [[d]] -> [[e]]
> zipWith2d4 = zipWith4 . zipWith4
>
> scanLeftward, scanRightward, scanUpward, scanDownward
>     :: (a -> b -> b) -> b -> [[a]] -> [[b]]
> 
> scanLeftward  f z = map (init . scanr f z)
> scanRightward f z = map (tail . scanl (flip f) z)
> scanUpward    f z = init . scanr (\as bs -> zipWith f as bs) (repeat z)
> scanDownward  f z = tail . scanl (\as bs -> zipWith f bs as) (repeat z)
> 
> -----------------------------------------------------------------------------
> -- Example grids
> -----------------------------------------------------------------------------
> 
> test4 = map (map (/='.')) $
>     [".#####.##...."
>     ,"###########.#"
>     ,"####.#.######"
>     ,"#############"
>     ,".#.##.######."
>     ,"####.###.##.."
>     ,"############."
>     ,"####..###...."
>     ,"#..###..####."]
> example = test4
>
> -- cases on which quickcheck failed before
> test5 = [[False,False,True,False,True,True,True]
>         ,[True,True,False,False,False,True,False]
>         ,[False,True,False,True,False,False,False]
>         ,[True,True,False,True,False,True,False]
>         ,[True,False,False,True,True,True,True]
>         ,[False,True,False,True,True,False,True]
>         ,[True,True,False,False,False,True,True]] 
> 
> test6 = [[True,False],[False,True],[False,True]]
> test7 = [[True,False,False],[False,True,False]]
> 
> disp :: Image -> IO ()
> disp = mapM_ (putStrLn . map s) where s x = if x then '#' else '.'
> 
> -----------------------------------------------------------------------------
> -- Drawing
> -----------------------------------------------------------------------------
> 
> -- See part2.

