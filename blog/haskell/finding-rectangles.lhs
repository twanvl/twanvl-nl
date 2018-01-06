title: Finding rectangles
subtitle: An efficient algorithm for finding rectangles in images.
date: 2011-09-28 21:49CEST
tag: haskell, rectangles
sourcelink: This post (including diagrams) is literate Haskell, click here to download the source code.

> -- HIDDEN
> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
> import Control.Monad
> import Data.Ord
> import Data.List
> import Data.Maybe
> import Data.Monoid
> import Test.QuickCheck
> import Graphics.GridDiagrams

This post is based on a part of my <a href="http://twanvl.nl/files/master-thesis/thesis-ocr-Twan-van-Laarhoven-2010-09-11.pdf">masters thesis</a>. The topic of my thesis was <abbr title="Optical Character Recognition">OCR</abbr> of historical documents.
A problem that came up there was the following:

<blockquote style="font-style:italic;">Given a binary image, find the largest axis aligned rectangle that consists only of foreground pixels.</blockquote>

These largest rectangles can be used, for instance, to find columns in a page of text. Although in that case one would use large rectangles of ''background'' pixels.

Here is an example image,
 <br><img src="image/find-rect/rects1-best.png" style="margin-left:2em;">.<br>
White pixels are background and blue is the foreground.
The rectangle with the largest area is indicated in red.
The images you encounter in practical application will be much larger than this example, so efficiency is going to be important.

-- Specification --

Let's start with the types of images and rectangles

> -- An image is a 2D list of booleans, True is the foreground
> type Image = [[Bool]]
> 
> -- An axis-aligned rectangle
> data Rect = Rect { left, top, width, height :: Int }
>     deriving (Eq,Ord,Show)

And some properties of them,

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

I will say that an image 'contains' a rectangle if all pixels inside the rectangle are foreground pixels.

> contains :: Image -> Rect -> Bool
> contains im (Rect x y w h) = and pixelsInRect
>   where
>     pixelsInRect = concatMap cols (rows im)
>     rows = take h . drop y . (++repeat [])
>     cols = take w . drop x . (++repeat False)

Now the obvious, stupid, way of finding the largest rectangle is to enumerate ''all'' rectangles in the image, and pick the largest from that list:

> -- List all rectangles contained in an image
> allRects :: Image -> [Rect]
> allRects im = filter (im `contains`) rects
>   where
>     rects = [Rect x y w h | x <- [0..iw], y <- [0..ih]
>                           , w <- [1..iw-x], h <- [1..ih-y]]
>     iw = imWidth im
>     ih = imHeight im

For now, I will take 'largest rectangle' to mean one with the maximal area.
I will come back to this choice soon.

> largestRect__spec :: Image -> Rect
> largestRect__spec = maximalRectBy area . allRects
> 
> -- Return the rectangle with maximum f,
> --  using lexicographical ordering to break ties
> --  return noRect if there are no rectangles in the input list.
> maximalRectBy :: Ord a => (Rect -> a) -> [Rect] -> Rect
> maximalRectBy f = maximumBy (comparing f `mappend` compare) . (noRect:)
>   where noRect = Rect 0 0 0 0

The above code should hopefully be easy to understand. It will find the correct answer for the above example:

]> λ> largestRect__spec example
]> Rect {left = 3, top = 2, width = 4, height = 5}

Of course @largestRect__spec@ is horribly slow.
In an $n$ by $n$ image there are $O(n^4)$ rectangles to consider, and checking if one is contained in the image takes $O(n^2)$ work, for a total of $O(n^6)$.

-- What is 'largest'? --

Before continuing, let's determine what it means for a rectangle to be the ''largest''.
We could compare the area of rectangles, as we did before. But it is equally valid to look for the rectangle with the largest perimeter.

Can we pick the maximum according to any arbitrary function @f :: (Rect -> a)@?
Not all of these functions will correspond to the intuitive notion of 'largest'. For example @f = negate . area@ will actually lead to the smallest rectangle.
In general there is going to be no efficient way of finding the rectangle that maximizes @f@. All we could do is optimize @contains@, to get an $O(n^4)$ algorithm.

We should therefore restrict @f@ to be ''monotonic''.
What I mean by monotonic is that @f x >= f y@ whenever rectangle @x@ contains rectangle @y@.
In QuickCheck code:

> prop_isMonotonic :: Ord a => (Rect -> a) -> Property
> prop_isMonotonic f = property $ \x y ->  x `rectContains` y  ==>  f x >= f y
> 
> rectContains :: Rect -> Rect -> Bool
> rectContains (Rect x y w h) (Rect x' y' w' h') = x <= x' && y <= y' && x+w >= x'+w' && y+h >= y'+h'

Area is a monotonic function, and so is perimeter.
But you could also add weird constraints. For example, only consider rectangles that are at least 10 pixels tall, or only rectangles that contain the point (123,456).

Maximizing a monotonic function, as opposed to just any function, means that we can skip a lot of rectangles.
In particular, whenever rectangle @x@ contains rectangle @y@, rectangle @y@ doesn't need to be considered.
I will call rectangles in the image that are not contained in other (larger) rectangles ''maximal''.
The strategy for finding the largest rectangle is then simply to enumerate only the maximal rectangles, and pick the best of those:

> largestRect__fast :: Image -> Rect
> largestRect__fast = maximalRectBy area . allMaximalRects

For each maximal rectangle there is (trivially) a monotonic function that is maximal for that rectangle. So we can't do any better without taking the specific function @f@ into account.

-- Machinery --

To find maximal rectangles, we are first of all going to need some machinery for working with images.
In particular, zipping images together,

> zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
> zip2d = zipWith zip
> 
> zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
> zipWith2d = zipWith . zipWith
> 
> zipWith2d4 :: (a -> b -> c -> d -> e) -> [[a]] -> [[b]] -> [[c]] -> [[d]] -> [[e]]
> zipWith2d4 = zipWith4 . zipWith4

And accumulating/scanning over images.
This scanning can be done in four directions. Each @scanX@ function takes a function to apply, and the initial value to use just outside the image. The scans that I use here are slightly different from @scanl@ and @scanr@, because the output will have the same size as the input, instead of being one element larger.

> scanLeftward, scanRightward, scanUpward, scanDownward
>     :: (a -> b -> b) -> b -> [[a]] -> [[b]]
> 
> scanLeftward  f z = map (init . scanr f z)
> scanRightward f z = map (tail . scanl (flip f) z)
> scanUpward    f z = init . scanr (\as bs -> zipWith f as bs) (repeat z)
> scanDownward  f z = tail . scanl (\as bs -> zipWith f bs as) (repeat z)

Here is an example of a scan that calculates the x-coordinate of each pixel,

]> -- BLOCK: haskell-stepwise
]> let x = scanRightward (\a x -> x + 1) (-1) a
]> x = !!!<img src="image/find-rect/rects1-x.png" style="vertical-align:middle;">!!!.

And the y-coordinates are of course

]> -- BLOCK: haskell-stepwise
]> let y = scanDownward (\a x -> x + 1) (-1) a
]> y = !!!<img src="image/find-rect/rects1-y.png" style="vertical-align:middle;">!!!.

-- Finding lines --

If we were looking for one-dimensional images, then a 'rectangle' would just be a single line of pixels.
Now each pixel is contained in at most one maximal line of foreground pixels.
To find the coordinates of this line, we just need to know the left and right endpoints.

For a foreground pixel, the left endpoint of the line it is in is the same as the left endpoint of its left neighbor.
On the other hand, a background pixel is not in any foreground line. So the left endpoint of all lines to the right of it will be at least @x+1@, where @x@ is the x-coordinate of the background pixel.
In both these cases information flows from left to right; and so the left endpoint for all pixels can be determined with a rightward scan.

Unsurprisingly, we can find the right endpoints of all foreground lines with a leftward scan.
Now let's do this for all lines in the image.
Notice that we need the @x@ coordinates defined previously: 

]> -- BLOCK: haskell-stepwise
]> let l = scanRightward (\(a,x) l -> if a then l else x+1) 0 (zip2d a x)
]> l = !!!<img src="image/find-rect/rects1-lm.png" style="vertical-align:middle;margin-bottom:1em;">!!!
]> let r = scanLeftward (\(a,x) r -> if a then r else x) (imWidth a) (zip2d a x)
]> r = !!!<img src="image/find-rect/rects1-rm.png" style="vertical-align:middle;">!!!

In the images I have marked the left and right endpoints of the foreground lines in red.
Note also, the values in the background pixels are not important, and you should just ignore them.

Vertically we can of course do the same thing, giving top and bottom endpoints:

]> -- BLOCK: haskell-stepwise
]> let t = scanDownward (\(a,y) t -> if a then t else y+1) 0 (zip2d a y)
]> t = !!!<img src="image/find-rect/rects1-tm.png" style="vertical-align:middle;margin-bottom:1em;">!!!
]> let b = scanUpward (\(a,y) b -> if a then b else y) (imHeight a) (zip2d a y)
]> b = !!!<img src="image/find-rect/rects1-bm.png" style="vertical-align:middle;">!!!

However, combining these left/right/top/bottom line endpoints does not yet give rectangles containing only foreground pixels.
Rather, it gives something like a cross. For example using the endpoints for (6,4) leads to the following incorrect rectangle,
 <br><img src="image/find-rect/rects1-cross.png" style="margin-left:2em;">.

In fact, there are many rectangles around this point (6,4):
 <br><img src="image/find-rect/rects1-all.png" style="margin-left:2em;">,
<br>and before looking at the area (or whatever function we are maximizing) there is way no telling which is the best one.

If there was some way to find just a single maximal rectangle for each pixel, then we would have an $O(n^2)$ algorithm. Assuming of course that we do find all maximal rectangles.

-- Finding maximal rectangles --

Suppose that @Rect x y w h@ is a maximal rectangle. What does that mean?
First of all, one of the points above the rectangle, @(x,y-1),(x+1,y-1),..,(x+w-1,y-1)@, must not be the a foreground pixel.
Because if all these points are foreground, then the rectangle could be extended upwards, and it would not be maximal.
So, suppose that @(u,y-1)@ is a background pixel (or outside the image).
Then @(u,y)@ is the top endpoint of the vertical line that contains @(u,y+h-1)@.

If we start from @(u,v)@, we can recover the height of a maximal rectangle using the top endpoint image @t@.
Just take @t!!(u,v)@ as the top coordinate, and @u+1@ as the bottom.
This image illustrates the idea:
 <br><img src="image/find-rect/rects1-extend-up.png" style="margin-left:2em;">.
<br>Here the green point @(u,v)@ has the red top endpoint, and it gives the height and vertical position of the yellow maximal rectangle.

Then to make this vertical line into a maximal rectangle, we just extend it horizontally as far as possible:
 <br><img src="image/find-rect/rects1-extend-lr.png" style="margin-left:2em;">.

For this last step, we need to know the first background pixel that will be encountered when extending the rectangle to the left. That is the ''maximum value'' of all left endpoints in the rows @t,t+1,..,b-1@.
This maximum can again be determined with a scan over the image:

]> -- BLOCK: haskell-stepwise
]> let lt = scanDownward (\(a,l) lt -> if a then max l lt else minBound) minBound (zip2d a l)
]> lt = !!!<img src="image/find-rect/rects1-lt.png" style="vertical-align:middle;">!!!

For extending to the right the story is exactly the same, only taking the minimum right endpoint instead:

]> let rt = scanDownward (\(a,r) rt -> if a then min r rt else maxBound) maxBound (zip2d a r)
]> rt = !!!<img src="image/find-rect/rects1-rt.png" style="vertical-align:middle;">!!!

Now we have all the ingredients for finding maximal rectangles:
 * For a foreground pixel @(u,v)@:
 * Take as top @t!!(u,v)@
 * Take as left @lt!!(u,v)@
 * Take as right @rt!!(u,v)@
 * Take as bottom @v+1@.

Every maximal rectangle can be found in this way.
However, not all rectangles we get in this way are maximal. In particular, they could potentially still be extended downward. However, for finding the largest rectangle, it doesn't matter if we also see some non-maximal ones. There might also be duplicates, which again does not matter.

So now finishing up is just a matter of putting all the steps together in a function:

> allMaximalRects :: Image -> [Rect]
> allMaximalRects a = catMaybes . concat $ zipWith2d4 mkRect lt rt t y
>   where
>     x  = scanRightward (\_ x -> x + 1) (-1) a
>     y  = scanDownward  (\_ y -> y + 1) (-1) a
>     l  = scanRightward (\(a,x) l -> if a then l else x+1) 0 (zip2d a x)
>     r  = scanLeftward  (\(a,x) r -> if a then r else x) (imWidth a) (zip2d a x)
>     t  = scanDownward  (\(a,y) t -> if a then t else y+1) 0 (zip2d a y)
>     lt = scanDownward  (\(a,l) lt -> if a then max l lt else minBound) minBound (zip2d a l)
>     rt = scanDownward  (\(a,r) rt -> if a then min r rt else maxBound) maxBound (zip2d a r)
>     mkRect l r t y
>         | l /= minBound = Just $ Rect l t (r-l) (y-t+1)
>         | otherwise     = Nothing

A quick QuickCheck shows that @largestRect__fast@ finds the same answer as the slow specification:

> prop__fast_spec = forAll genImage $ \a -> largestRect__spec a == largestRect__fast a

]> λ> quickCheck prop__fast_spec
]> +++ OK, passed 100 tests.


-- Conclusion --

It is possible to find all maximal rectangles that consist entirely of foreground pixels in an $n*n$ image in $O(n^2)$ time. That is linear in the number of pixels. Obviously it is not possible to do any better in general.

You may wonder whether this method also works in higher dimensions. And the answer to that question is ''no''.
The reason is that there can be more than $O(n^3)$ maximal cubes in a three dimensional image. In fact, there can be at least $O(n^{(d-1)^2})$ maximal hypercubes in $d$ dimensions. Just generalize this image to 3D:
 <br><img src="image/find-rect/worst-case.png" style="margin-left:2em;">.
<a href="http://kyucon.com/qblock/#29920">Or click here for a 3D version</a>.

> -- HIDDEN
> genImage :: Gen Image
> genImage = sized $ \n ->
>   do w <- choose (0,n)
>      h <- choose (0,n)
>      vectorOf h $ vectorOf w arbitrary
> 
> instance Arbitrary Rect where
>     arbitrary = liftM4 Rect arbitrary arbitrary (liftM abs arbitrary) (liftM abs arbitrary)

> -- HIDDEN
> 
> onlyTrulyMaximalRects :: [Rect] -> [Rect]
> onlyTrulyMaximalRects xs = filter (\x -> not $ any (\y -> y /= x && y `rectContains` x) xs) xs
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
>     setLineWidth size
>     fillStroke (alpha 0.05 color) color
> 
> drawHEdges = drawRowsWith $ mapM_ $ \x ->
>     when (x `notElem` [minBound,maxBound])
>        $ translated (fromIntegral x) 0 $ drawBorder H red 3
> 
> drawVEdges = (drawColsWith $ mapM_ $ \y ->
>     when (y `notElem` [minBound,maxBound])
>        $ translated 0 (fromIntegral y) $ drawBorder V red 3) . transpose
> 
> -----------------------------------------------------------------------------
> -- Example grids
> -----------------------------------------------------------------------------
> 
> test1, test2, example :: [[Bool]]
> test1 = map (map (=='1')) $
>     ["01101111"
>     ,"11001111"
>     ,"11111111"
>     ,"10111101"
>     ,"00111110"
>     ,"10111101"
>     ,"11111111"
>     ,"11110111"]
> test2 = map (map (=='1')) $
>     ["1011011110"
>     ,"1110011110"
>     ,"0111111111"
>     ,"0101111010"
>     ,"0001111100"
>     ,"0101111011"
>     ,"0111111111"
>     ,"0111101110"]
> test3 = map (map (=='1')) $
>     ["1011001110"
>     ,"1110011110"
>     ,"0111111111"
>     ,"0101111110"
>     ,"0011111100"
>     ,"0101111011"
>     ,"0111111111"
>     ,"0111101110"]
> worstCase = map (map (=='1')) $
>     ["1111100000"
>     ,"1111000000"
>     ,"1110000000"
>     ,"1100000000"
>     ,"1000000000"
>     ,"0000000001"
>     ,"0000000011"
>     ,"0000000111"
>     ,"0000001111"
>     ,"0000011111"]
> qcFail1 = [[False,True,False,True,True,False,True,True],[True,False,False,False,True,False,True,False]]
> qcFail2 = [[True,False,False,True,True,False,True,True,True],[False,False,False,True,False,True,False,True,True],[False,False,True,True,True,False,False,True,False],[False,True,False,False,False,True,True,False,False],[True,False,False,True,True,True,True,False,False],[False,True,False,True,False,False,True,True,False],[True,True,False,True,False,False,True,False,False]]
> example = test3
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
>     let lt = scanDownward  (\(a,l) lt -> if a then max l lt else minBound) minBound (zip2d a l)
>     let rt = scanDownward  (\(a,r) rt -> if a then min r rt else maxBound) maxBound (zip2d a r)
>     let wt = zipWith2d (-) rt lt
>     let w  = zipWith2d (-) r l
>     let ifA = zipWith2d (\a b -> if a then b else minBound) a
>     let largeRect = largestRect__spec a
>     render (fn++"-a")  $ draw a
>     render (fn++"-x")  $ draw (zip2d a x)
>     render (fn++"-y")  $ draw (zip2d a y)
>     render (fn++"-l")  $ draw (zip2d a l)
>     render (fn++"-r")  $ draw (zip2d a r)
>     render (fn++"-t")  $ draw (zip2d a t)
>     render (fn++"-b")  $ draw (zip2d a b)
>     render (fn++"-lm") $ draw (zip2d a l) >> drawHEdges (ifA l)
>     render (fn++"-rm") $ draw (zip2d a r) >> drawHEdges (ifA r)
>     render (fn++"-tm") $ draw (zip2d a t) >> drawVEdges (ifA t)
>     render (fn++"-bm") $ draw (zip2d a b) >> drawVEdges (ifA b)
>     render (fn++"-la") $ draw (zip2d a (ifA l)) >> drawHEdges (ifA l)
>     render (fn++"-ra") $ draw (zip2d a (ifA r)) >> drawHEdges (ifA r)
>     render (fn++"-ta") $ draw (zip2d a (ifA t)) >> drawVEdges (ifA t)
>     render (fn++"-ba") $ draw (zip2d a (ifA b)) >> drawVEdges (ifA b)
>     render (fn++"-lt") $ draw (zip2d a lt) >> drawHEdges lt
>     render (fn++"-rt") $ draw (zip2d a rt) >> drawHEdges rt
>     render (fn++"-wt") $ draw (zip2d a wt)
>     render (fn++"-w")  $ draw (zip2d a (ifA w))
>     render (fn++"-best") $ draw a >> drawRect largeRect 3 red
>     -- draw a 'cross' at pixel (cx,cy)
>     when (imWidth a > 6 && imHeight a > 4) $ do
>      let (cx,cy) = (6,4)
>      let [cl,cr,ct,cb] = map (\u -> u !! cy !! cx) [l,r,t,b]
>      render (fn++"-cross") $ do
>         let a' = zipWith3 (zipWith3 color) a x y
>             color a x y
>                 | considered      = alpha 0.25 black `over` toColor a
>                 | otherwise       = alpha 0.8  $ toColor a
>               where considered = (x == cx && y >= ct && y < cb)
>                               || (y == cy && x >= cl && x < cr)
>                     inside     = x >= cl && x < cr && y >= ct && y < cb
>         draw a'
>         translated (fromIntegral cl) (fromIntegral cy) $ drawBorder H red 3
>         translated (fromIntegral cr) (fromIntegral cy) $ drawBorder H red 3
>         translated (fromIntegral cx) (fromIntegral ct) $ drawBorder V red 3
>         translated (fromIntegral cx) (fromIntegral cb) $ drawBorder V red 3
>         setDash [5,2] 0.5
>         drawRect (Rect cl ct (cr-cl) (cb-ct)) 1 red
>      -- draw all maximal rectangles around a pixel (cx,cy)
>      render (fn++"-all") $ do
>         let rects = filter (`rectContains` Rect cx cy 1 1)
>                   . nub
>                   . onlyTrulyMaximalRects
>                   . allMaximalRects $ a
>         draw a
>         translated (fromIntegral cx) (fromIntegral cy) $ draw (alpha 0.5 black)
>         setLineJoin LineJoinBevel
>         --forM_ rects $ \r -> drawRect r 2 red
>         --forM_ rects $ \r -> drawRect r 1 (alpha 0.5 black `over` red)
>         sequence_ $ zipWith (\r c -> drawRect r 2 c) rects (cycle [red,green,yellow,cyan,magenta,white])
>     when (imWidth a > 4 && imHeight a > 6) $ do
>      let (cx,cy) = (4,6)
>      let [clt,crt,ct,cb] = map (\u -> u !! cy !! cx) [lt,rt,t,b]
>      render (fn++"-extend-up") $ do
>         draw a
>         drawRect largeRect 1 (yellow)
>         translated (fromIntegral cx) (fromIntegral ct) $ drawBorder V red 3
>         translated (fromIntegral cx) (fromIntegral cy) $ draw (alpha 0.5 green)
>         drawRect (Rect cx ct 1 (cy-ct+1)) 1 (alpha 0.2 red)
>         drawRect (Rect cx cy 1 1) 1 green
>         setLineWidth 3
>         setColor red
>         --arrow (fromIntegral cx+0.5) (fromIntegral cy+0.5) (fromIntegral cx+0.5) (fromIntegral ct+0.5)
>         --arrow (fromIntegral cx+0.5) (fromIntegral cy-0.1) (fromIntegral cx+0.5) (fromIntegral ct+0.2)
>         arrow (fromIntegral cx+0.5) (fromIntegral cy+0.5) (fromIntegral cx+0.5) (fromIntegral ct+0.2)
>      render (fn++"-extend-lr") $ do
>         draw a
>         translated (fromIntegral cx) (fromIntegral ct) $ drawBorder V red 3
>         translated (fromIntegral clt) (fromIntegral ct+1) $ drawBorder H yellow 3
>         translated (fromIntegral clt) (fromIntegral ct+3) $ drawBorder H yellow 3
>         translated (fromIntegral crt) (fromIntegral ct+3) $ drawBorder H yellow 3
>         translated (fromIntegral cx) (fromIntegral cy) $ draw (alpha 0.5 green)
>         drawRect (Rect cx ct 1 (cy-ct+1)) 1 (red)
>         drawRect largeRect 1 (yellow)
>         setLineWidth 3
>         setColor yellow
>         let y = fromIntegral (ct+cy+1) / 2
>         arrow (fromIntegral cx-0.1) y (fromIntegral clt+0.2) y
>         arrow (fromIntegral cx+1.1) y (fromIntegral crt-0.2) y
> 
> arrow x1 y1 x2 y2 = do
>     let len = sqrt ( (x1-x2)^2 + (y1-y2)^2 )
>     let headSize1 = 0.4
>     let headSize2 = 0.35
>     let x3 = x1 + (x2-x1) * (len-headSize1)/len
>     let y3 = y1 + (y2-y1) * (len-headSize1)/len
>     let dx = (y1-y2)/len * headSize2
>     let dy = (x1-x2)/len * headSize2
>     line x1 y1 x2 y2
>     line x2 y2 (x3+dx) (y3+dy)
>     line x2 y2 (x3-dx) (y3-dy)
>     setLineCap LineCapRound
>     stroke
> 
> main = do
>     renderImages "rects1" example
>     render "worst-case" $ draw worstCase
> 
> render fn
>    = renderToFile' ("../../image/find-rect/"++fn++".png") defaultRenderOpts{ gridSize = 20 }
>   -- >> renderToFile' ("../../image/find-rect/"++fn++".svg") defaultRenderOpts{ gridSize = 20 }

