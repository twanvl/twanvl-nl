title: Knight in n, part 1: moves
subtitle: How many ways are there to move a knight on a chessboard from a to b?
date: 2008-11-26
tags: haskell, knight-in-n
sourcelink: This post is literate Haskell, click here to download the source code.

Consider the following problem:

<blockquote style='font-style:italic'>
<>A knight is placed at the origin of a chessboard that is infinite in all directions.
<>How many ways are there for that knight to reach cell $(i,j)$ in exactly $n$ moves?
</blockquote>

This ''knight moves problem'' is not hard, nor does it have any real life applications. The problem is still interesting because there are many different ways to solve it, ranging from very simple to quite complex. 
In this series of articles I will describe some of these solutions.

== Knight's moves ==

<img src="image/knight/chessboard1.png" style="float:right;margin-left:1em;" alt="The possible moves for a chess knight">

In chess, a knight can move two squares horizontally and one square vertically, or two squares vertically and one square horizontally. One complete move therefore looks like the letter 'L'.
The picture on the right shows all possible moves for the black knight in the center.


> -- IGNORE
> module Knight1 where
> 
> import Data.Array

We can summarize all these moves in an array:

> moves :: [(Int,Int)]
> moves = [(2,1),(2,-1),(-2,1),(-2,-1)
>         ,(1,2),(-1,2),(1,-2),(-1,-2)]

Counting the number of paths to $(i,j)$ in $n$ steps can now be done with a simple recursive function.
The base case is that in $0$ moves only cell $(0,0)$ is reachable. In the recursion step we simply try all moves:

> paths__rec :: Int -> (Int,Int) -> Integer
> paths__rec 0 (0,0) = 1
> paths__rec 0 (_,_) = 0
> paths__rec n (i,j) = sum [ paths__rec (n-1) (i+δ__i,j+δ__j) | (δ__i,δ__j) <- moves ]

So for example
]> Knight1> paths_rec 4 (2,2)
]> 54
I.e. there are 54 ways to reach cell $(2,2)$ in $4$ moves.


Unfortunately the function @paths__rec@ is not very efficient. In fact, it is very much not efficient.
At each step all $8$ possible moves are considered, so the total time complexity of this function is $O(8^n)$.


== Tables ==

Besides calculating the number of paths to a single point it can also be interesting to display the number of pats for each possible end point. We can make a list of lists containing all the path counts,

> pathMatrix paths n
>     = [ [ paths n (i,j) | j <- [-2*n .. 2*n] ] | i <- [-2*n .. 2*n] ]

and then display this list in a tabular format

> showMatrix :: Show α => [[α]] -> String
> showMatrix xss = unlines [ unwords [ show x | x <- xs ] | xs <- xss ]
> 
> printPathMatrix paths = putStr . showMatrix . pathMatrix paths

The path matrix for $n=1$ should be familiar, it is the same as the image of possible moves of a knight.

]> Knight1> printPathMatrix paths__rec 1
]>     0 1 0 1 0
]>     1 0 0 0 1
]>     0 0 0 0 0
]>     1 0 0 0 1
]>     0 1 0 1 0

But now we can also make larger tables:

]> Knight1> printPathMatrix paths__rec 2
]>     0 0 1 0 2 0 1 0 0
]>     0 2 0 2 0 2 0 2 0
]>     1 0 0 0 2 0 0 0 1
]>     0 2 0 2 0 2 0 2 0
]>     2 0 2 0 8 0 2 0 2
]>     0 2 0 2 0 2 0 2 0
]>     1 0 0 0 2 0 0 0 1
]>     0 2 0 2 0 2 0 2 0
]>     0 0 1 0 2 0 1 0 0

If you were to continue increasing $n$, the table and the numbers in it become ever larger.
It is a good idea to make a 'density plot', i.e. to use colors to visualize larger numbers. For example for $n=4$, the path matrix can be rendered as:
<br><img src="image/knight/knight-density4.png" alt="" style="vertical-align:middle;margin-left:2em;margin-top:3px;">

== Special cases ==

Looking at the above matrices, you might start to see some patterns emerge:
* A knight cannot move more than $2n$ cells in any direction (horizontal or vertical).
* Similarly, the knight moves no more than $3n$ squares in total.
* A knight always moves from a white square to a black square and vice-versa. In other words, the parity of $i+j+n$ must be zero.

These observations can be used as additional cases in the recursive function to quickly eliminate large parts of the input space:

> paths__case :: Int -> (Int,Int) -> Integer
> paths__case 0 (0,0) = 1
> paths__case 0 (_,_) = 0
> paths__case n (i,j) | (n+i+j) `mod` 2 /= 0 = 0
> paths__case n (i,j) | abs i + abs j > 3*n  = 0
> paths__case n (i,j) | abs i > 2*n          = 0
> paths__case n (i,j) | abs j > 2*n          = 0
> paths__case n (i,j) = sum [ paths__case (n-1) (i+δ__i,j+δ__j) | (δ__i,δ__j) <- moves ]

A quick test shows that this can be a big improvement for the run time:

]> Knight1> paths__rec 8 (4,4)
]> 124166
]> (92.88 secs, 4605991724 bytes)
]> Knight1> paths__case 8 (4,4)
]> 124166
]> (17.69 secs, 807191624 bytes)

The asymptotic time complexity of @paths__case@ is harder to analyze.
It is still $O(8^n)$ in the worst case, but the complexity is now also output dependant.


<br>That is all for now, next time we will look at smarter algorithms.
For the interested reader I would suggest that you try to come up with some ideas of your own. I would love to hear how other people approach this problem.

