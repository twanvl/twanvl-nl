title: Conduits vs. Pipes
subtitle: One pipe to rule them all
date: 2012-03-24 15:31CET
tags: haskell, pipes

Michael Snoyman released <a href="http://hackage.haskell.org/package/conduit-0.3.0">conduit-0.3</a> this week. The conduit package provides three datatypes that can be chained together: Source, Counduit and Sink. If you were to look at the source code, you will notice that there is a lot of overlap between these datatypes. In this post I'll show how these types can be combined into a single one, which is the idea used by the pipes package.

Compare:

> data Sink i m o =
>     Processing (i -> Sink i m o) (SinkClose m o)
>   | Done (Maybe i) o
>   | SinkM (m (Sink i m o))
> type SinkClose m o = m o

> data Conduit i m o =
>     NeedInput (i -> Conduit i m o) (ConduitClose m o)
>   | HaveOutput (Conduit i m o) (m ()) o
>   | Finished (Maybe i)
>   | ConduitM (m (Conduit i m o)) (m ())
> type ConduitClose m o = Source m o

The differences between the two types are that:
 * @Done@ returns output @o@, whereas @Finished@ does not.
 * @Conduit@ has a @HaveOutput@ constructor, while a @Sink@ does not.
 * @ConduitM@ has an 'early close' action of type @m ()@.
 * @SinkClose@ just gives a result, while @ConduitClose@ can return an entire stream in the form of a @Source m o@

The term output is in fact used differently by the two types, it becomes clearer when we say that @Sink@ has a ''result'' of type @r@. Then the result of @Conduit@ is @r = ()@. On the other hand, a sink doesn't produce output to downstream conduits, so its ''output'' type would be @Void@.

Now let's also bring in @Source@,

> data Source m a =
>     Open (Source m a) (m ()) a
>   | Closed
>   | SourceM (m (Source m a)) (m ())

The @SourceM@ constructor is exactly analogous to @ConduitM@, and @Open@ is analogous to @HaveOutput@. A @Source@ doesn't have input, so there is no analogue to @NeedInput@ or @Processing@. The @Closed@ constructor doesn't provide remaining input or result, since a source doesn't have either. However, we could say that its input is @i = ()@, and its result is @r = ()@.

It then becomes possible to unify the three datatypes into:

> data Pipe m i o r =
>     NeedInput (i -> Pipe m i o r) (Pipe m () o r)
>   | HaveOutput (Pipe m i o r) (m ()) o
>   | Finished (Maybe i) r
>   | PipeM (m (Pipe m i o r)) (m r)
> 
> type Source m o = Pipe m () o ()
> type Conduit i m o = Pipe m i o ()
> type Sink i m r = Pipe m i Void r

This is almost exactly the type provided by the various <a href="http://hackage.haskell.org/package/pipes-core">incarnations of</a> the <a href="http://hackage.haskell.org/package/pipes">pipes</a> package!

The three composition operators of conduits become a single operator on pipes. The top level "run" operation takes a @Pipe m () Void r@, that is, a (composition of) pipes that takes no input and has no output.

What about the instances for @Source@, @Conduit@ and @Sink@? In the conduit package @Sink@ is an instance of @Monad@ and its superclasses. That is also the case for @Pipe@.
@Source@ and @Conduit@ are instances of @Functor@, which allows you to map a function over the output. The output is no longer the last type variable of @Pipe@. Instead we should provide an instance of @Functor2@ or @Bifunctor@, which have a method @fmap2 :: (a -> b) -> f a r -> f b r@.

Overall, reducing the number of datatypes from 3 to 1 sounds like a pretty good deal to me. I therefore think it would be great if conduit adopted the ideas from pipes.

