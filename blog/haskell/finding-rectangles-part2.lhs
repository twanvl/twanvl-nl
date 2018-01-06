title: Finding rectangles, part 2: borders
subtitle: An efficient algorithm for finding rectangles in images.
date: 2011-10-12 09:57CEST
tag: haskell, rectangles
sourcelink: This post (including diagrams) is literate Haskell, click here to download the source code.

> -- HIDDEN
> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
> import Control.Monad
> import Control.Monad.Trans
> import Data.Ord
> import Data.List
> import Data.Monoid
> import Graphics.GridDiagrams
> import Test.QuickCheck

In <a href="blog/haskell/finding-rectangles">the previous post</a>, we looked at finding axis aligned rectangles in a binary image. Today I am going to solve a variation of that problem:

<blockquote style="font-style:italic;">
Given a binary image, find the largest axis aligned rectangle with a 1 pixel wide border that consists entirely of foreground pixels.
</blockquote>

Here is an example:
 <br><img src="image/find-rect/rects2-best.png" style="margin-left:2em;">,<br>
where white pixels are the background and blue is the foreground.
The rectangle with the largest area is indicated in red.

Like the previous rectangle finding problem, this one also came up in my masters thesis.
The application was to, given a scan of a book, find the part that is a page, cutting away clutter: 
 <br><img src="image/find-rect/rects2-page069-border-rect-small.png" style="margin-left:2em;">.


-- Specification --

The types we are going to need are exactly the same as in <a href="blog/haskell/finding-rectangles">my previous post</a>:

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

The difference compared to last time is the @contains@ function, which tells whether an image contains a given rectangle.
We are now looking only at the borders of rectangles, or 'border rectangles' for short.

> -- does an image contain a given border rectangle?
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
> isBorder im
>     = and (head im)     -- top border
>    && and (last im)     -- bottom border
>    && and (map head im) -- left border
>    && and (map last im) -- right border

Finding the largest border rectangle can again be done by enumerating all rectangles contained in the image, and picking the largest one:

> largestRect__spec :: Image -> Rect
> largestRect__spec = maximalRectBy area . allRects
> 
> allRects :: Image -> [Rect]
> allRects im = filter (im `contains`) rects
>   where -- boring details omitted, see previous post

> -- HIDDEN
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

Just as before, this specification has runtime $O(n^6)$ for an $n$ by $n$ image, which is completely impractical.


-- An $O(n^4)$ algorithm --

Unfortunately, the nice properties of maximal rectangles will not help us out this time.
In particular, whenever a filled rectangle is contained in an image, then so are all smaller subrectangles
So we could 'grow' filled rectangles one row or column at a time.
This is no longer true for border rectangles.

We can, however, easily improve the above $O(n^6)$ algorithm to an $O(n^4)$ one by using the line endpoints.
With those we can check if an image contains a rectangle in constant time.
We just need to check all four of the sides:

]> -- BLOCK: haskell-pseudo-code
]> -- pseudo code, not actually O(n^4) without O(1) array lookup
]> contains__fast im (Rect x y w h)
]>     = r!!(x,y)     >= x+w -- top border
]>    && r!!(x,y+h-1) >= x+w -- bottom border
]>    && b!!(x,y)     >= y+h -- left border
]>    && b!!(x+w-1,y) >= y+h -- right border

Where @r@ and @b@ give the right and bottom endpoints of the horizontal and vertical lines through each pixel.
 <br>@r = @<img src="image/find-rect/rects2-r.png" style="vertical-align:middle;">,
     @b = @<img src="image/find-rect/rects2-b.png" style="vertical-align:middle;">.


-- An $O(n^3)$ algorithm --

As the title of this section hints, a still more efficient algorithm is possible.
The trick is to only look for rectangles with a specific height @h@.
For any given height @h@, we ''will'' be able to find only maximal rectangles of that height.

For example, for @h=6@ we would expect to find these rectangles:
 <br><img src="image/find-rect/rects2-all6.png" style="margin-left:2em;">.<br>
Notice how each of these rectangles consist of three parts: a left side, a middle and a right side:
 <br><img src="image/find-rect/rects2-lmr6.png" style="margin-left:2em;">.

The left and right parts both consist of a vertical line at least @h@ pixels high.
We can find those vertical lines by looking at the top (or bottom) line endpoints. The top endpoint for pixel @(x,y+h-1)@ should be at most @y@,

]> let h = 6 -- for example
]> let av = zipWith2d (<=) (drop (h-1) t) y
]> -- in images:
]> av = !!!<img src="image/find-rect/rects2-av6-snd.png" style="vertical-align:middle;">!!! <= !!!<img src="image/find-rect/rects2-av6-fst.png" style="vertical-align:middle;">!!!
]>    = !!!<img src="image/find-rect/rects2-av6.png" style="vertical-align:middle;margin-top:1em;">!!!

Each @True@ pixel in @av@ corresponds to a point where there is a @h@ pixel high vertical line. So, a potential left or right side of a rectangle.

The middle part of each rectangle has both pixel @(x,y)@ and @(x,y+h-1)@ set,

]> let ah = zipWith2d (&&) a (drop (h-1) a)
]> -- in images:
]> ah = !!!<img src="image/find-rect/rects2-ah6-snd.png" style="vertical-align:middle;">!!! && !!!<img src="image/find-rect/rects2-ah6-fst.png" style="vertical-align:middle;">!!!
]>    = !!!<img src="image/find-rect/rects2-ah6.png" style="vertical-align:middle;margin-top:1em;">!!!

To find the rectangles of height @h@, we just need to find runs that start and end with a pixel in @av@, and where all pixels in between are in @ah@.
First we find the left coordinates of the rectangles,

]> let leStep (av,ah,x) le
]>       | av = min le x  -- pixel in av ==> left part
]>       | ah = le        -- pixel in ah, but not av ==> continue middle part
]>       | otherwise = maxBound
]> let le = scanRightward leStep maxBound (zip2d3 av ah x)
]> le = !!!<img src="image/find-rect/rects2-lh6.png" style="vertical-align:middle;">!!!

Finally we need to look for right sides. These are again given by @av@.
For each right side, @le@ gives the leftmost left side, and @h@ gives the height of the rectangles:

]> let mkRect x y av le
]>       | av = [Rect le y (x-le+1) h] -- pixel in av ==> right part
]>       | otherwise = []
]> let rects = zipWith2d4 mkRect x y av le
]> rects = !!!<img src="image/find-rect/rects2-lh-rects6.png" style="vertical-align:middle;">!!!

Compare the resulting image to the one at the start of this section. We found the same rectangles.

Just like last time, all we need to do now is put the steps together in a function:

> rectsWithHeight :: Int -> Image -> [Rect]
> rectsWithHeight h a = concat . concat $ rects
>   where
>     x  = scanRightward (\_ x -> x + 1) (-1) a
>     y  = scanDownward  (\_ y -> y + 1) (-1) a
>     t  = scanDownward  (\(a,y) t -> if a then t else y+1) 0 (zip2d a y)
>     ah = zipWith2d (&&) (drop (h-1) a) a
>     av = zipWith2d (<=) (drop (h-1) t) y
>     leStep (av,ah,x) le
>       | av = min le x
>       | ah = le
>       | otherwise = maxBound
>     le = scanRightward leStep maxBound (zip2d3 av ah x)
>     mkRect x y av le
>       | av = [Rect le y (x-le+1) h]
>       | otherwise = []
>     rects = zipWith2d4 mkRect x y av le

> -- HIDDEN
> rectsWithHeight__spec :: Int -> Image -> [Rect]
> rectsWithHeight__spec h = filter ((== h) . height) . allRects

//]> quickCheck $ \h -> forAll genImage $ \a -> rectsWithHeight__fast h a == rectsWithHeight__spec h a

Of course, finding (a superset of) all maximal rectangles in an image is just a matter of calling @rectsWithHeight@ for all possible heights.

> findRects__fast :: Image -> [Rect]
> findRects__fast im = concat [ rectsWithHeight h im | h <- [1..imHeight im] ]
> 
> largestRect__fast :: Image -> Rect
> largestRect__fast = maximalRectBy area . findRects__fast

Let's quickly check that this function does the same as the specification,

> prop__fast_spec = forAll genImage $ \a -> largestRect__spec a == largestRect__fast a

]> λ> quickCheck prop__fast_spec
]> +++ OK, passed 100 tests.

Great.


-- Conclusions --

The runtime of @rectsWithHeight@ is linear in the number of pixels; and it is called $n$ times for an $n$ by $n$ image. Therefore the total runtime of @largestRect__fast@ is $O(n^3)$. While this is much better than what we started with, it can still be quite slow. For example, the book page that motivated this problem is around 2000 pixels squared. Finding the largest rectangle takes on the order of $2000^3 = 8*10^9$, or 8 giga-operations, which is still a pretty large number.

To make this algorithm faster in practice, I used a couple of tricks. Most importantly, if we know what function we are maximizing, say @area@, then we can stop as soon as we know that we can't possibly find a better rectangle. The idea is to start with @h=imHeight im@, and work downwards. Keep track of the area @a@ of the largest rectangle. Then as soon as @h * imWidth im < a@, we can stop, because any rectangle we can find from then on will be smaller.

Is this the best we can do? No. I know an algorithm for finding all maximal border rectangles in $O(n^2*(log n)^2)$ time. But it is rather complicated, and this post is long enough already. So I will save it for another time. If anyone thinks they can come up with such an algorithm themselves, I would love to read about it in the comments.

> -- HIDDEN
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
> test1, example :: [[Bool]]
> test1 = map (map (=='1')) $
>     ["1011001110"
>     ,"1110011110"
>     ,"0111111111"
>     ,"0101111110"
>     ,"0011111100"
>     ,"0101111011"
>     ,"0111111111"
>     ,"0111101110"]
> blank = map (map (/='.')) $
>     ["............"
>     ,"............"
>     ,"............"
>     ,"............"
>     ,"............"
>     ,"............"
>     ,"............"
>     ,"............"
>     ,"............"]
> test2 = map (map (/='.')) $
>     [".##.##.##.#."
>     ,"##########.#"
>     ,"##.#...#####"
>     ,"############"
>     ,".#.###.#####"
>     ,"##.#.#.#.##."
>     ,"###########."
>     ,"####...##..."
>     ,"#..#########"]
> test3 = map (map (/='.')) $
>     [".#####.##..."
>     ,"##########.."
>     ,"####.#.#####"
>     ,"############"
>     ,".#.##.######"
>     ,"####.###.##."
>     ,"###########."
>     ,"####...##..."
>     ,"#..###..####"]
> test4 = map (map (/='.')) $
>     ["..####.##...."
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
> disp :: Image -> IO ()
> disp = mapM_ (putStrLn . map s) where s x = if x then '#' else '.'
> 
> -----------------------------------------------------------------------------
> -- Drawing
> -----------------------------------------------------------------------------
> 
> toColor :: Bool -> Color
> toColor False = white
> toColor True  = rgb 0.4 0.6 1
> 
> instance Drawable Bool where
>     draw = draw . toColor
> 
> instance Drawable Int where
>     draw i | i `elem` [minBound,maxBound] = return ()
>     draw i = draw (show i)
> 
> drawRect (Rect x y w h) size color = do
>     rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
>     when (w>1 && h>1) $
>         rectangle (fromIntegral x + 1) (fromIntegral y + 1) (fromIntegral w - 2) (fromIntegral h - 2)
>     setLineWidth size
>     setFillRule FillRuleEvenOdd
>     fillStroke (alpha 0.25 color) color
> 
> drawHEdges color = drawRowsWith $ mapM_ $ \x ->
>     when (x `notElem` [minBound,maxBound])
>        $ translated x 0 $ drawBorder H color 3
> 
> drawVEdges color = (drawColsWith $ mapM_ $ \y ->
>     when (y `notElem` [minBound,maxBound])
>        $ translated 0 y $ drawBorder V color 3) . transpose
> 
> data Side = T | L | B | R deriving (Eq,Enum)
> rotate n xs = drop n xs ++ take n xs
> 
> drawBracket side r@(Rect x y w h) size color = do
>     let x1 = if side == L then x else x+1
>     let y1 = if side == T then y else y+1
>     let x2 = if side == R then x+w else x+w-1
>     let y2 = if side == B then y+h else y+h-1
>     -- fill
>     rectangle x y w h
>     when (w>1 && h>1) $ rectangle x1 y1 (x2-x1) (y2-y1)
>     setFillRule FillRuleEvenOdd
>     setColor (alpha 0.33 color) >> fill
>     --stroke
>     path $ rotate (fromEnum side) [(x,y),(x,y+h),(x+w,y+h),(x+w,y)]
>     path $ rotate (fromEnum side) [(x1,y1),(x1,y2),(x2,y2),(x2,y1)]
>     setLineWidth size
>     setColor color >> stroke
> 
> {-
> drawSqCap r@(Rect x y w h) size color = do
>     -- fill
>     rectangle x y w h
>     when (w>1 && h>1) $ rectangle (x+1) (y+1) (w-2) (h-1)
>     setFillRule FillRuleEvenOdd
>     setColor (alpha 0.33 color) >> fill
>     --stroke
>     path [(x,y+h),(x,y),(x+w,y),(x+w,y+h)]
>     path [(x+1,y+h),(x+1,y+1),(x+w-1,y+1),(x+w-1,y+h)]
>     setLineWidth size
>     setColor color >> stroke
> 
> drawSqCup r@(Rect x y w h) size color = do
>     -- fill
>     rectangle x y w h
>     when (w>1 && h>1) $ rectangle (x+1) (y) (w-2) (h-1)
>     setFillRule FillRuleEvenOdd
>     setColor (alpha 0.33 color) >> fill
>     --stroke
>     path [(x,y),(x,y+h),(x+w,y+h),(x+w,y)]
>     path [(x+1,y),(x+1,y+h-1),(x+w-1,y+h-1),(x+w-1,y)]
>     setLineWidth size
>     setColor color >> stroke
> -}
> drawSqCap = drawBracket B
> drawSqCup = drawBracket T
> 
> drawMidline y w = do
>     px <- getPixelSize
>     line (-px) (realToFrac y) (realToFrac w + px) (realToFrac y)
>     setDash [2,2] 0.5
>     setColor yellow >> setLineWidth 3 >> stroke
>     setDashSolid
> 
> drawMidline' rect = do
>     setDash [2,2] 0.5
>     drawRect rect 3 yellow
>     setDashSolid
> 
> -----------------------------------------------------------------------------
> -- Main function
> -----------------------------------------------------------------------------
> 
> renderImages fn a = do
>     let x  = scanRightward (\_ x -> x + 1) (-1) a :: [[Int]]
>     let y  = scanDownward  (\_ y -> y + 1) (-1) a :: [[Int]]
>     let l  = scanRightward (\(a,x) l -> if a then l else x+1) 0 (zip2d a x)
>     let r  = scanLeftward  (\(a,x) r -> if a then r else x)   (imWidth a) (zip2d a x)
>     let t  = scanDownward  (\(a,y) t -> if a then t else y+1) 0 (zip2d a y)
>     let b  = scanUpward    (\(a,y) b -> if a then b else y)   (imHeight a) (zip2d a y)
>     let h = 6
>     let ah = zipWith2d (&&) a (drop (h-1) a)
>     let av = zipWith2d (>=) y (drop (h-1) t)
>     let lh = scanRightward (\(av,ah,x) l -> if av then min l x
>                                         else if ah then l
>                                         else maxBound)  maxBound (zip2d3 av ah x)
>     let largeRect = largestRect__spec a
>     let ifA = zipWith2d (\a b -> if a then b else minBound) a
>     let ahv = zipWith2d (\ah av -> alpha (if av then 0.3 else 0) black `over` toColor ah) ah av
>     let rects = zipWith2d4 (\lh av x y -> if av then [Rect lh y (x-lh+1) h] else []) lh av x y
>     render (fn++"-a")  $ draw a
>     render (fn++"-best") $ draw a >> drawRect largeRect 3 red
>     render (fn++"-l")  $ draw (zip2d a l) >> drawHEdges red (ifA l)
>     render (fn++"-r")  $ draw (zip2d a r) >> drawHEdges red (ifA r)
>     render (fn++"-t")  $ draw (zip2d a t) >> drawVEdges red (ifA t)
>     render (fn++"-b")  $ draw (zip2d a b) >> drawVEdges red (ifA b)
>     -- new in this part
>     render (fn++"-ah"++show h)  $ draw ah
>     render (fn++"-av"++show h)  $ draw av
>     render (fn++"-ahv"++show h)  $ draw ahv
>     -- construction of ah
>     let text str = do
>             g <- getGridSize
>             selectFontFace "Ubuntu" FontSlantNormal FontWeightNormal
>             setFontSize (g * 0.95)
>             setColor black
>             translated 0 (-0.1) $ showTextCentered str
>     render (fn++"-ah"++show h++"-fst") $ do
>         translated 0 (h - 1) $ draw a
>         rectangle 0 0 (imWidth a) (imHeight a + h - 1) >> setLineWidth 3 >> extend
>         px <- getPixelSize
>         setLineWidth 0
>         rectangle (-px) (realToFrac (imHeight a) + px) (realToFrac (imWidth a)+2*px) (realToFrac h - 1 + 1*px)
>         setColor (alpha 0.7 white) >> fill
>         translated (realToFrac (imWidth a) / 2) (realToFrac (imHeight a) + realToFrac (h-1) / 2) $
>             text "bottom h-1 lines are ingored"
>     render (fn++"-ah"++show h++"-snd") $ do
>         draw a
>         rectangle 0 0 (imWidth a) (imHeight a + h - 1) >> setLineWidth 3 >> extend
>         px <- getPixelSize
>         setLineWidth 0
>         rectangle (-px) (-px) (realToFrac (imWidth a)+2*px) (realToFrac h - 1 + 1*px)
>         setColor (alpha 0.7 white) >> fill
>         translated (realToFrac (imWidth a) / 2) (realToFrac (h-1) / 2) $
>             text "dropped the top h-1 lines"
>     -- construction of av
>     render (fn++"-av"++show h++"-fst") $ do
>         translated 0 (h - 1) $ draw [[(False,y)] | y <- [0..imHeight a - h]]
>         rectangle 0 0 1 1 >> setLineWidth 3 >> extend
>     render (fn++"-av"++show h++"-snd") $ do
>         draw (zip2d a t)
>         px <- getPixelSize
>         setLineWidth 0
>         rectangle (-px) (-1.5*px) (realToFrac (imWidth a)+2*px) (realToFrac h - 1 + 1*px)
>         setColor (alpha 0.7 white) >> fill
>         drawVEdges (alpha 0.5 red `over` white) (take h $ ifA t)
>         drawVEdges red (drop h $ ifA t)
>         translated (realToFrac (imWidth a) / 2) (realToFrac (h-1) / 2) $ 
>             text "dropped the top h-1 lines"
>     -- lh...
>     render (fn++"-lh"++show h)  $ draw (zip2d ahv lh) >> drawHEdges red lh
>     render (fn++"-lh-rects"++show h) $ do
>         draw (zip2d ahv lh)
>         --drawHEdges lh
>         let mr = sort $ onlyTrulyMaximalRects (concat $ concat rects)
>         forM_ (zip mr (cycle [red,green,yellow,cyan,magenta,white])) $ \(Rect x y w _,c) -> do
>             sp <- liftM (1.5*) getPixelSize
>             rectangle (realToFrac x+sp) (realToFrac y+sp) (realToFrac w-2*sp) (realToFrac 1-2*sp)
>             setLineWidth 2
>             fillStroke (alpha 0.25 c) c
>     render (fn++"-all"++show h) $ do
>         draw a
>         let rects = nub . onlyTrulyMaximalRects . rectsWithHeight__spec h $ a
>         --let rects = nub . filter ((>0).width) . rectsWithHeight__spec h $ a
>         sequence_ $ zipWith (\r c -> drawRect r 2 c) rects (cycle [red,green,yellow,cyan,magenta,white])
> 
> -- images for fast algorithm
> renderImagesPart3a fn a = do -- UNUSED
>     let largeRect = largestRect__spec a
>     let (Rect x y w h) = largeRect
>     let m = imHeight a `div` 2
>     let midline = (Rect 0 m (imWidth a) 1)
>     let base = do
>         draw a
>         setDash [2,2] 0.5
>         drawRect midline 2 yellow
>         setDashSolid
>     render (fn++"-mid") $ base
>     render (fn++"-sqcap") $ do
>         base
>         drawSqCap (Rect x y w (m-y+1)) 3 red
>     render (fn++"-sqcup") $ do
>         base
>         drawSqCup (Rect x m w (y+h-m)) 3 red
> 
> renderImagesPart3b fn a = do
>     let largeRect = largestRect__spec a
>     let (Rect x y w h) = largeRect
>     let m = imHeight a `div` 2
>     let base = do
>         draw a
>         drawMidline m (imWidth a)
>     render (fn++"-vmid") $ base
>     render (fn++"-sqcap") $ do
>         base
>         drawSqCap (Rect x y w (m-y)) 3 red
>     render (fn++"-sqcup") $ do
>         base
>         drawSqCup (Rect x m w (y+h-m)) 3 red
>     renderImagesCup (fn++"-sqcup") (drop m a)
> 
> renderImagesCup fn a = do
>     let x  = scanRightward (\_ x -> x + 1) (-1) a :: [[Int]]
>     let y  = scanDownward  (\_ y -> y + 1) (-1) a :: [[Int]]
>     let l  = scanRightward (\(a,x) l -> if a then l else x+1) 0 (zip2d a x)
>     let r  = scanLeftward  (\(a,x) r -> if a then r else x)   (imWidth a) (zip2d a x)
>     let b  = scanUpward    (\(a,y) b -> if a then b else y)   (imHeight a) (zip2d a y)
>     render (fn++"-b") $ do
>         draw a
>         drawMidline 0 (imWidth a)
>         drawCells (take 1 b)
>         drawVEdges red (take 1 b)
>     let i=1
>     let ri=map (!!i) r
>     render (fn++"-r"++show i) $ do
>         draw a
>         drawMidline 0 (imWidth a)
>         translated i 0 $ drawRowsWith draw ri
>         drawHEdges red (map return ri)
> 
> renderImagesPart3c fn a = do
>     let largeRect = largestRect__spec a
>     let (Rect x y w h) = largeRect
>     let m = imWidth a `div` 2
>     let r  = scanLeftward  (\(a,x) r -> if a then r else x)   (imWidth a) (zip2d a x)
>                 where x  = scanRightward (\_ x -> x + 1) (-1) a :: [[Int]]
>     let b  = scanUpward    (\(a,y) b -> if a then b else y)   (imHeight a) (zip2d a y)
>                 where y  = scanDownward  (\_ y -> y + 1) (-1) a :: [[Int]]
>     let base = do
>         draw a
>         drawMidline' (Rect m 0 1 (imHeight a))
>     render (fn++"-mid") $ base
>     render (fn++"-lbracket") $ do
>         base
>         drawBracket R (Rect x y (m-x+1) h) 3 red
>     render (fn++"-rbracket") $ do
>         base
>         drawBracket L (Rect m y (x+w-m) h) 3 red
>     render (fn++"-rbracket-conditions-full") $ do
>         draw a
>         -- dim left halve of image
>         px <- getPixelSize
>         setLineWidth 0
>         rectangle (-px) (-px) (realToFrac m+1*px) (realToFrac (imHeight a) + 2*px)
>         setColor (alpha 0.7 white) >> fill
>         -- midline
>         setDash [2,2] 0.5
>         drawRect (Rect m 0 1 (imHeight a)) 3 (alpha 0.6 yellow)
>         setDashSolid
>         -- bracket
>         drawBracket L (Rect m y (x+w-m) h) 1 (alpha 0.4 white `over` red)
>         -- borders
>         translated (r!!y!!m) y             $ drawBorder H green 3
>         translated (r!!(y+h-1)!!m) (y+h-1) $ drawBorder H green 3
>         translated (x+w-1) (b!!y!!(x+w-1)) $ drawBorder V red 3
>         setColor (alpha 0.5 black `over` green)
>         translated m y                     $ drawStringCell $ show (r!!y!!m)
>         translated m (y+h-1)               $ drawStringCell $ show (r!!(y+h-1)!!m)
>         setColor (alpha 0.3 black `over` red)
>         translated (x+w-1) y               $ drawStringCell $ show (b!!y!!(x+w-1))
>     render (fn++"-rbracket-conditions") $ do
>         draw (map (drop m) a)
>         -- midline
>         setDash [2,2] 0.5
>         drawRect (Rect 0 0 1 (imHeight a)) 3 (alpha 0.6 yellow)
>         setDashSolid
>         -- bracket
>         drawBracket L (Rect 0 y (x+w-m) h) 1 (alpha 0.4 white `over` magenta)
>         -- borders
>         translated (r!!y!!m - m) y             $ drawBorder H green 3
>         translated (r!!(y+h-1)!!m - m) (y+h-1) $ drawBorder H green 3
>         translated (x+w-1 - m) (b!!y!!(x+w-1)) $ drawBorder V red 3
>         setColor (alpha 0.4 black `over` green)
>         translated 0 y                     $ drawStringCell $ show (r!!y!!m - m)
>         translated 0 (y+h-1)               $ drawStringCell $ show (r!!(y+h-1)!!m - m)
>         -- borders
>         setColor (alpha 0.3 black `over` red)
>         translated (x+w-1 - m) y           $ drawStringCell $ show (b!!y!!(x+w-1))
>     renderImagesRBracket (fn++"-rbracket") (map (drop m) a)
>     render (fn++"-recurse") $ do
>         draw a
>         setDash [2,2] 0.5
>         let drawMid x y w h (color:colors)
>              | w == 0 || h == 0 = return ()
>              | w <= 3 && h <= 3 = return ()
>              | w >= h = do
>                  let m = w `div` 2
>                  drawMid x y m h colors
>                  drawMid (x+m+1) y (w-m-1) h colors
>                  drawRect (Rect (x+m) y 1 h) 1 color
>              | otherwise = do
>                  let m = h `div` 2
>                  drawMid x y w m colors
>                  drawMid x (y+m+1) w (h-m-1) colors
>                  drawRect (Rect x (y+m) w 1) 1 color
>         drawMid 0 0 (imWidth a) (imHeight a) [yellow,green,magenta,red,white,black,cyan,yellow]
> 
> renderImagesRBracket fn a = do
>     let x  = scanRightward (\_ x -> x + 1) (-1) a :: [[Int]]
>     let y  = scanDownward  (\_ y -> y + 1) (-1) a :: [[Int]]
>     let l  = scanRightward (\(a,x) l -> if a then l else x+1) 0 (zip2d a x)
>     let r  = scanLeftward  (\(a,x) r -> if a then r else x)   (imWidth a) (zip2d a x)
>     let b  = scanUpward    (\(a,y) b -> if a then b else y)   (imHeight a) (zip2d a y)
>     render (fn++"-r") $ do
>         draw a
>         drawMidline' (Rect 0 0 1 (imHeight a))
>         drawCells (map (take 1) r)
>         drawHEdges red (map (take 1) r)
>     let i=1
>     let bi=b!!i
>     render (fn++"-b"++show i) $ do
>         draw a
>         drawMidline' (Rect 0 0 1 (imHeight a))
>         translated 0 i $ drawColsWith draw bi
>         drawVEdges red [bi]
> 
> -- left / middle / right split
> renderLMR fn a = do
>     let hh = 6
>     render (fn++"-lmr"++show hh)  $ do
>         let r@(Rect x y w h) = maximalRectBy area $ rectsWithHeight__spec hh $ a
>         draw a
>         setLineWidth 3
>         -- left
>         rectangle x y 1 h
>         setColor (alpha 0.5 red) >> fill
>         moveTo (x+1) y
>         path [(x+1,y),(x,y),(x,y+h),(x+1,y+h)]
>         path [(x+1,y+1),(x+1,y+h-1)]
>         setColor red >> stroke
>         -- right
>         rectangle (x+w-1) y 1 h
>         setColor (alpha 0.5 green) >> fill
>         path [(x+w-1,y),(x+w,y),(x+w,y+h),(x+w-1,y+h)]
>         path [(x+w-1,y+1),(x+w-1,y+h-1)]
>         setColor green >> stroke
>         -- middle
>         rectangle (x+1) y (w-2) 1
>         rectangle (x+1) (y+h-1) (w-2) 1
>         setColor (alpha 0.5 yellow) >> fill
>         forM_ [y,y+1,y+h-1,y+h] $ \yy -> line (x+1) yy (x+w-1) yy
>         setColor yellow >> stroke
> 
> main = do
>     renderImages "rects2" example
>     renderLMR "rects2" example
>     renderImagesPart3b "rects2" example
>     renderImagesPart3c "rects2" example
> 
> render fn
>    = renderToFile' ("../../image/find-rect/"++fn++".png") defaultRenderOpts{ gridSize = 20, bgColor = transparent }

