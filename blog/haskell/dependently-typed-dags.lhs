title: Dependently typed DAGs
tags: haskell
subtitle: Using GADTs to get a safe representation of Directed Acyclic Graphs.
date: 2012-03-19 22:16CET
sourcelink: This post is literate Haskell, click here to download the source code.

A colleague of mine recently needed to represent <a href="http://en.wikipedia.org/wiki/Directed_acyclic_graph">DAGs</a> (directed acyclic graphs) in Coq, and asked around for ideas. Since Coq is not a nice language to program in, I decided to use Haskell instead.
Something close to dependently typed programming is possible in Haskell thanks to GADTs. And other extensions will be helpful too,

> {-# LANGUAGE GADTs, TypeOperators, Rank2Types #-}

My idea is to represent a DAG as a list of nodes.
Nodes have a list of children, where each child is a reference to an element <em>later</em> in the list.


For example, the DAG<br>
 <img src="image/dag/tree1.png" style="margin-left:2em;"><br>
would be represented as
// <br><img src="image/dag/dag-repr1.png" style="margin-left:2em;">,<br>
//or in code as:
]> --BLOCK: haskell-pseudo-code
]> [Node "a" [1,2,2,4], Node "b" [3,3], Node "c" [3,4], Node "d" [], Node "e" []]

-- Data types --

To make the above representation safe, we need to ensure two things:
 * Each child-reference is greater than the index of the parent.
 * Each child-reference refers to an actual node, so it must be smaller than the size of the list.

The first condition is easily satisfied, by making the reference <em>relative to the current position</em> and using natural numbers. So the representation would be
]> --BLOCK: haskell-pseudo-code
]> [Node "a" [0,1,1,3], Node "b" [1,1], Node "c" [0,1], Node "d" [], Node "e" []]
For the second condition we need dependent types. In particular the type @Fin n@ of numbers smaller than @n@.

> data Zero
> data Succ n
> 
> data Fin n where
>     Fin0 :: Fin (Succ n)
>     FinS :: Fin n -> Fin (Succ n)

A node then holds a label of type @a@ and a list of numbers less than @n@.

> data Node a n where
>     Node :: a -> [Fin n] -> Node a n
>   deriving (Eq,Show)

For the list of nodes we will use a dependently typed vector,

> data Vec f n where
>     Empty :: Vec f Zero
>     (:::) :: f n -> Vec f n -> Vec f (Succ n)
> infixr 5 :::

A value of @Vec f n@ is a list of the form @[]@ or @[x__0::f 0]@ or [x__1::f 1, x__0::f 0]@ or [x__2::f 2, x__1::f 1, x__0::f 0]@ etc., with a length equal to the parameter @n@.
That is exactly what we need for DAGs:

> type DAG a = Vec (Node a)

-- Instances --

I would like to define @Eq@ and @Show@ instances for these datatypes. But the instance for @Vec@ would look something like
]> instance (forall m. Eq (f m)) => Eq (Vec f n)
which is not valid Haskell, even with extensions. The solution is to use another class, @Eq1@

> class Eq1 f where
>     eq1 :: f n -> f n -> Bool

Now we can define

> instance Eq1 f => Eq (Vec f n) where
>     Empty == Empty = True
>     (x ::: xs) == (y ::: ys) = eq1 x y && xs == ys

The boring instances for @Node@ and @Fin@ are

> instance Eq a => Eq1 (Node a) where
>     eq1 = (==)
> instance Eq1 Fin where
>     eq1 = (==)
> instance Eq1 f => Eq1 (Vec f) where
>     eq1 = (==)
> 
> -- ghc can't derive this
> instance Eq (Fin n) where
>     Fin0   == Fin0   = True
>     FinS i == FinS j = i == j
>     _      == _      = False


The same goes for @Show@

> class Show1 a where
>     showsPrec1 :: Int -> a n -> ShowS
> 
> -- instances ommitted, see source code

> -- IGNORE
> instance Show1 f => Show (Vec f n) where
>     showsPrec _ Empty = showString "Empty"
>     showsPrec _ (x ::: xs) = showsPrec1 0 x . showString " ::: " . showsPrec1 0 xs
> instance Show a => Show1 (Node a) where
>     showsPrec1 = showsPrec
> instance Show1 f => Show1 (Vec f) where
>     showsPrec1 = showsPrec
> instance Show (Fin n) where
>     show = show . toInt
> 
> toInt :: Fin n -> Int
> toInt Fin0 = 0
> toInt (FinS i) = toInt i + 1


-- Convert to tree --

To show that these DAGs work, we can convert from a DAG to a tree by duplicating all nodes.
The tree type is a simple rose tree, as those in Data.Tree:

> data Tree a = TNode a [Tree a]  deriving Show

To be able to make a dag into a tree, we need to know the root node. So we give the @toTree@ a @DAG n@ and an @Fin n@ to indicate the root.

> -- Convert a DAG to a tree, using the given node index as root
> toTree :: Fin n -> DAG a n -> Tree a
> toTree Fin0 (Node x cs ::: ns) = TNode x [toTree c ns | c <- cs]
> toTree (FinS i) (_ ::: ns) = toTree i ns -- drop the head until we reach the root

And for convenience, a function that assumes that the first node in the list is the root.

> toTree' :: DAG a (Succ n) -> Tree a
> toTree' = toTree Fin0

Here is the example from above

> example = Node "a" [Fin0,FinS Fin0,FinS Fin0,FinS (FinS (FinS (Fin0)))]
>       ::: Node "b" [FinS Fin0,FinS Fin0]
>       ::: Node "c" [Fin0,FinS Fin0]
>       ::: Node "d" []
>       ::: Node "e" []
>       ::: Empty

]> λ> toTree' example
]> TNode "a" [TNode "b" [TNode "d" [],TNode "d" []]
]>           ,TNode "c" [TNode "d" [],TNode "e" []]
]>           ,TNode "c" [TNode "d" [],TNode "e" []]
]>           ,TNode "e" []]

As an image:<br><img src="image/dag/tree-expanded.png" style="margin-left:2em;">.

-- Convert from a tree --

More interesting is the conversion ''from'' a tree ''to'' a DAG, in such a way that we share identical nodes.
For that we first of all need to be able to search a DAG to see if it already contains a particular node.

Let's do this a bit more generic, and define a search over any @Vec f@.

> findVec :: (Eq1 f, Pred1 f) => f n -> Vec f n -> Maybe (Fin n)

What is that @Pred1@ class? And why do we need it? When you have a value of type @f n@, and you want to compare it to the elements of a vector, you will quickly discover that these elements have different types, @f m@ with @m < n@. So, we need to either convert the @f n@ to the @f m@ or vice-versa.

I'll go with the former, because that means the search can stop early. If a node refers to a child @Fin0@, that means it points to the first node in the DAG. So there is no point in looking if it is duplicated anywhere in vector, because other nodes can't possibly refer to earlier ones.

What the @Pred1@ class does is tell you: "if this item occurred one place later in the vector, what would it look like?". And if it can not occur in later places return @Nothing@:

> class Pred1 f where
>     pred1 :: f (Succ n) -> Maybe (f n)
> 
> instance Pred1 Fin where
>     pred1 Fin0 = Nothing
>     pred1 (FinS i) = Just i
> 
> instance Pred1 (Node a) where
>     pred1 (Node x cs) = Node x `fmap` mapM pred1 cs

Now the search becomes relatively straight forward:

> findVec x (y ::: ys) = case pred1 x of
>     Just x' | eq1 x' y  -> Just Fin0
>             | otherwise -> FinS `fmap` findVec x' ys
>     Nothing -> Nothing
> findVec _ _ = Nothing

The nice thing about GADTs is that it becomes almost impossible to make mistakes, because the typechecker will complain if you do.


-- Lifting boxes --

When converting a @Tree@ to a @DAG@, we do not know beforehand how many nodes that DAG is going to have.
Therefore, we need to put the produced @DAG@ into an existential box, that hides the parameter @n@.

That is fine for the end result, but it will not work when incrementally constructing a DAG.
Suppose you wanted to add two nodes to a DAG. Adding the first node is fine, but then you need to ensure that the children of the second node are still there. In addition, the second node will need to be adjusted: all child references have to be incremented, to skip the first added node.

That adjusting is done with the the counterpart to @Pred1@, the @Succ1@ class

> class Succ1 f where
>     succ1 :: f n -> f (Succ n)
> 
> instance Succ1 Fin where
>     succ1 = FinS
> 
> instance Succ1 (Node a) where
>     succ1 (Node x cs) = Node x (map FinS cs)

Our box will come with the ability to 'lift' any succable value into it:

> data Box f n where
>     Box :: (forall g. Succ1 g => g n -> g m) -> f m -> Box f n

> -- IGNORE
> -- boring instances
> instance Show1 f => Show1 (Box f) where
>     showsPrec1 p (Box _ x) = showsPrec1 p x
> instance Show1 f => Show (Box f a) where
>     showsPrec = showsPrec1

You can think of @Box f n@ as a value of @f m@ where @m >= n@. This allows turning any @g n@ into a @g m@, which can be combined with the value in the box.
Before we can see @Box@ in action, we will first need some functors to store things:

> -- product functor
> data (:*:) f g a = (:*:) { fst1 :: f a, snd1 :: g a }
> -- functor composition
> newtype (:.:) f g a = Comp { getComp :: f (g a) }

Now when adding a node we check if it is already in the DAG, and if so, return the index.
If the node is not yet in the DAG, then add it. By adding the node the DAG becomes 1 larger, from a @DAG n@ we get a @DAG (Succ n)@. Therefore, we need one level of @succ@.

> consNode :: Eq a => Node a n -> DAG a n -> Box (Fin :*: DAG a) n
> consNode n dag = case findVec n dag of
>     Just i  -> Box id    (i :*: dag)
>     Nothing -> Box succ1 (Fin0 :*: (n ::: dag))

Now the ugly part: converting an entire node.

> fromTree :: Eq a => Tree a -> DAG a n -> Box (Fin :*: DAG a) n
> fromTree (TNode x cs) dag__0
>  = case fromForest cs dag__0 of
>     Box to__1 (Comp cs__1 :*: dag__1) ->
>      case consNode (Node x cs__1) dag__1 of
>       Box to__2 ans -> Box (to__2 . to__1) ans

And a forest, aka. a list of trees:

> fromForest :: Eq a => [Tree a] -> DAG a n -> Box (([] :.: Fin) :*: DAG a) n
> fromForest [] dag = Box id $ Comp [] :*: dag
> fromForest (x:xs) dag__0
>    = case fromForest xs dag__0 of
>       Box to__1 (Comp xs__1 :*: dag__1) ->
>        case fromTree x dag__1 of
>         Box to__2 (x__2 :*: dag__2) ->
>          Box (to__2 . to__1) (Comp (x__2 : map to__2 xs__1) :*: dag__2)

At the top level we start with an empty DAG, and ignore the index of the root (which will always be Fin0).

> fromTree' :: Eq a => Tree a -> Box (DAG a) Zero
> fromTree' x
>   = case fromTree x Empty of
>      Box to__1 (_ :*: dag__1) ->
>       Box to__1 dag__1

To understand these functions, you should ignore the @Box@ constructors, what you are left with is

]> -- BLOCK: haskell-pseudo-code
]> fromTree__pseudo (TNode x cs) dag
]>     = let (cs',dag') = fromForest__pseudo cs dag
]>       in constNode (Node x cs') dag
]> fromForest__pseudo []     dag = dag
]> fromForest__pseudo (x:xs) dag
]>     = let (ns,dag') = fromForest__pseudo xs
]>           (n,dag'') = fromTree__pseudo x dag'
]>       in (n:ns,dag'')

Here is a test that shows that we are able to recover the sharing that was removed by @toTree'@:

]> λ> fromTree' (toTree' example)
]> Node "a" [0,1,1,3] ::: Node "b" [1,1] ::: Node "c" [0,1]
]>                    ::: Node "d" [] ::: Node "e" [] ::: Empty
]> λ> fromTree' (toTree' example)
]>       == Box (succ1 . succ1 . succ1 . succ1 . succ1) example
]> True


-- Box is a monad --

All this wrapping and unwrapping of @Box@ is really ugly. It should also remind you of something.
//You might have seen similar code when working with @Maybe@s, for instance.
That something is a monad. And @Box@ is indeed a monad, just not a normal Haskell one.
Instead it is (surprise, surprise) a 'Monad1':

> class Monad1 m where
>     return1 :: f a -> m f a
>     (>>>=) :: m f a -> (forall b. f b -> m g b) -> m g a
> 
> instance Monad1 Box where
>     return1 = Box id
>     Box l x >>>= f = case f x of
>         Box l' y -> Box (l' . l) y

Combine this with two utility functions:

> -- Lift a value y into a Box
> boxLift :: Succ1 g => Box f n -> g n -> Box (f :*: g) n
> boxLift (Box l x) y = Box l (x :*: l y)

> -- Apply one level of succ before putting things into a Box
> boxSucc :: Box f (Succ n) -> Box f n
> boxSucc (Box l x) = Box (l . succ1) x

And one more @Succ1@ instance:

> instance (Functor f, Succ1 g) => Succ1 (f :.: g) where
>     succ1 (Comp x) = Comp (fmap succ1 x)

Now we can write this slightly less ugly code

> fromTree__m :: Eq a => Tree a -> DAG a n -> Box (Fin :*: DAG a) n
> fromTree__m (TNode x cs) dag__0
>   = fromForest__m cs dag__0
>        >>>= \(Comp cs__1 :*: dag__1) ->
>     consNode (Node x cs__1) dag__1
> 
> fromForest__m :: Eq a => [Tree a] -> DAG a n -> Box (([] :.: Fin) :*: DAG a) n
> fromForest__m [] dag
>   = return1 $ Comp [] :*: dag
> fromForest__m (x:xs) dag__0
>   = fromForest__m xs dag__0
>        >>>= \(xs__1 :*: dag__1) ->
>     fromTree__m x dag__1 `boxLift` xs__1
>        >>>= \(x__2 :*: dag__2 :*: Comp xs__2) ->
>     return1 $ Comp (x__2 : xs__2) :*: dag__2

This might be even nicer when we add in a state monad for the @DAG@, but I'll leave that for (maybe) another time.

-- Bonus: alternative definition of Box --

If you don't like existential boxes, then here is another way to define the @Box@ monad.

> data Box' f n where
>     Box0 :: f n -> Box' f n
>     BoxS :: Box' f (Succ n) -> Box' f n
> 
> instance Monad1 Box' where
>     return1 = Box0
>     Box0 x >>>= y = y x
>     BoxS x >>>= y = BoxS (x >>>= y)
> 
> boxSucc' :: Box' f (Succ n) -> Box' f n
> boxSucc' = BoxS
> 
> boxLift' :: Succ1 g => Box' f n -> g n -> Box' (f :*: g) n
> boxLift' (Box0 x) y = Box0 (x :*: y)
> boxLift' (BoxS x) y = BoxS (boxLift' x (succ1 y))


The two types are equivalent, as shown by

> equiv__1 :: Box' f n -> Box f n
> equiv__1 (Box0 x) = return1 x
> equiv__1 (BoxS x) = boxSucc (equiv__1 x)
> 
> equiv__2 :: Box f n -> Box' f n
> equiv__2 (Box l x) = runUnBox' (l (UnBox' id)) (Box0 x)
> 
> newtype UnBox' f m n = UnBox' {runUnBox' :: Box' f n -> Box' f m}
> instance Succ1 (UnBox' r f) where
>     succ1 f = UnBox' (runUnBox' f . BoxS)


> -- IGNORE
> -- tricky instance
> instance Eq1 f => Eq (Box' f n) where
>     Box0 a == Box0 b = eq1 a b
>     BoxS a == BoxS b = a == b
>     _ == _ = False
> -- is there a more direct way to difine this?
> instance Eq1 f => Eq (Box f n) where
>     a == b = equiv__2 a == equiv__2 b

