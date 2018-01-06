date: 2015-10-11
title: Extra unsafe sequencing of IO actions
subtitle: Why write sane code when you can live on the edge instead?
tags: haskell, unsafe

Warning: evil ahead!

A while ago Neil Mitchell wrote about <a href="http://neilmitchell.blogspot.co.uk/2015/09/making-sequencemapm-for-io-take-o1-stack.html">a different implementation of @sequence@ for the @IO@ monad</a>.
The issue with the usual definition is that it is not tail recursive.
Neil's version uses some hacks to essentially break out of the IO monad. But the solution does require two traversals of the list.

Now in any language other than Haskell this IO monad wouldn't exist at all, and with a bit of luck lists would be mutable. Then you could implement @sequence@ by just appending items to the end of a list.
In Haskell you can not do that. Or can you?

A obvious way to implement mutable lists in haskell is with @IORef@s. But then you end up with something that is not an ordinary list, and you would have to use the IO monad even for reading from it.
Instead, why not be unsafe? Just because Haskell doesn't let you change the tail of a list doesn't mean that it is impossible.

Now obviously this requires something beyond ordinary haskell. And even doing it from C via the foreign function interface is hard, because GHC will try to marshall values you pass to C functions. But GHC also allows you to write primitive operations in C--, which is essentially a portable assembly language. In C-- you *can* just overwrite the tail pointer of a @(:)@ constructor to point to something else.

So I wrote a simple @unsafeSetField@ function.
> --LANGUAGE: c--
> unsafeSetFieldzh (W_ i, gcptr x, gcptr y) {
>   W_ bd;
>   x = UNTAG(x);
>   P_[x + SIZEOF_StgHeader + WDS(i)] = y; // write in memory
>   
>   bd = Bdescr(x);
>   if (bdescr_gen_no(bd) != 0 :: bits16) {
>     recordMutableCap(x, TO_W_(bdescr_gen_no(bd)));
>     return ();
>   } else {
>     return ();
>   }
> }

There are several things going on here. First of all, GHC uses <a href="https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/PointerTagging">pointer</a> tagging, meaning that we need to untag the incomming pointer. Secondly, it might be the case that the @x@ lives in the <a href="https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC/RememberedSets">old GC generation</a>, in which case we have to mark the fact that we changed it, since otherwise @y@ might get garbage collected.
By the way, the @zh@ in the end of the function name is <a href="https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames">the z encoding</a> for the @#@ character.

Now to use this function from Haskell we import it and add some @unsafeCoerce@,

> -- LANGUAGE: haskell
> foreign import prim "unsafeSetFieldzh" unsafeSetField#
>   :: Int# -> Any -> Any -> (##)
> 
> unsafeSetField :: Int -> a -> b -> IO ()
> unsafeSetField (I# i) !x y =
>   case unsafeSetField# i (unsafeCoerce# x :: Any) (unsafeCoerce# y :: Any) of
>     (##) -> return ()
> {-# INLINEABLE unsafeSetField #-}

With it we can implement @sequence@ as follows

> sequenceU :: [IO a] -> IO [a]
> sequenceU [] = return []
> sequenceU (mx0:xs0) = do
>     x0 <- mx0
>     let front = x0:[]
>     go front xs0
>     return front
>   where
>   go back [] = return ()
>   go back (mx:xs) = do
>     x <- mx
>     let back' = x:[]
>     unsafeSetField 1 back back'
>     go back' xs
> {-# INLINEABLE sequenceT #-}

Now for the big questions: Does it work? The answer is that, yes it does!
Benchmarking shows that the unsafe @sequenceU@ is between 11% and 23% faster than Neil's @sequenceIO@ in all cases.
For small lists the standard @sequence@ implementation is still marginally faster.

You should be aware that GHC sometimes shares values, so overwriting part of one might overwrite them all. And also that constant lists might become static values, meaning not allocated on the heap. So trying to overwrite parts of those will just crash the program. 

I also wouldn't be surprised if the above code is subtly wrong. Perhaps I am missing a write barrier or doing something wrong with the generation check. So I wouldn't use it in production code if I were you.


What if you don't even know beforehand what constructor to use?
The GHC runtime system has something called <a href="https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#Indirections">indirections</a>. These are used to replace a thunk with its result after evaluation. But we can also use indirections to replace a value itself. But because of pointer tagging you can't just replace one constructor by another, because the tag would be wrong.

Instead the idea is to first allocate a special "hole" value, and then later fill that hole by overwriting it with an indirection. Note that you can only do that once, because the runtime system will follow and remove indirections when possible. So you get an API that looks like
> newHole :: IO a
> setHole :: a -> a -> IO a
It is also possible to implement @sequence@ with holes. But, perhaps unsurprisingly, this turns out to be a bit slower.
I'll leave the actual implementation as an exercise for the interested reader, as well as the question of what other evil you can commit with it.


I wanted to publish the functions from this post on hackage, but unfortunately I haven't yet figured out how to include C-- files in a cabal package. So instead everything is on <a href="https://github.com/twanvl/unsafe-sequence">github</a>.

