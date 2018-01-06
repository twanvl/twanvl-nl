title: Arrays without bounds
subtitle: A Haskell array datatype that grows as needed.
date: 2008-11-14
tags: haskell
sourcelink: This post is literate Haskell, click here to download the source code.

Regular old arrays have a size; you can't just have an infinite array.
On the other hand, a lazy language such as Haskell does allow infinite lists.
The idea behind the @UnboundedArray@ module is to combine the $O(1)$ access of arrays with the unbounded size of lazy lists.

> module UnboundedArray where

This data type is built on top of ordinary arrays and unsafe IO operations:

> import Data.Array
> import Data.IORef
> import System.IO.Unsafe

To keep things simple, an unbounded array is just a function from the natural numbers to array elements:

> type UnboundedArray a = Int -> a


I am just going to dump the code here instead of explaining it.
The idea is to make an array and resize it when it becomes too small.
If the size increases geometrically with each resize, then the amortized cost of a single access will be $O(1)$.

> -- | Create an unbounded array from an infinite list
> --   Accessing element /n/ takes /O(n)/ time, but only /O(1)/ amortized time.
> unboundedArray :: [a] -> UnboundedArray a
> unboundedArray xs = unsafePerformIO . unsafePerformIO (unboundedArrayIO xs)
> 
> unboundedArrayIO :: [a] -> IO (Int -> IO a)
> unboundedArrayIO xs = do
>     theArray <- newIORef (listArray (0,0) xs)
>     return $ \n -> do
>         ar <- readIORef theArray
>         let (0,size) = bounds ar
>         if n <= size
>           then return $ ar ! n
>           else do let size' = max n (size * 3 `div` 2)
>                   let ar' = listArray (0,size') xs
>                   writeIORef theArray ar'
>                   return $ ar' ! n

So, what are UnboundedArrays good for?
A simple application is memoization, for example:

> memo__Int f = unboundedArray (map f [0..])
> 
> fib = memo__Int realFib
>   where realFib 0 = 1
>         realFib 1 = 1
>         realFib n = fib (n - 1) + fib (n - 2)

]> > map fib [1..20]
] [1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946]

But since we can use an arbitrary list for initialization the @unboundedArray@ function can sometimes be more flexible/convenient than @memo__Int@.
