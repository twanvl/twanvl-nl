title: More composition operators
subtitle: Variations of the trusted (.) operator
date: 2010-07-13
tags: haskell
sourcelink: This post is literate Haskell, click here to download the source code.

Here is an idea for some more function composition operators, beyond just @(.)@:

> (f .$ g) x = (f) . (g $ x)
> (f $. g) x = (f $ x) . (g)
> (f .$$ g) x y = (f) . (g $ x $ y)
> (f $.$ g) x y = (f $ x) . (g $ y)
> (f $$. g) x y = (f $ x $ y) . (g)
> -- etc.
> infixl 8 .$, $., .$$, $.$, $$. -- slightly less tight than (.)

The @.$@ name is supposed suggests that an extra argument is applied on the right before the functions are composed. Notice also that the dollars and dot on the left hand site match those on the right hand side.
These combinators make writing point free code easier:

]> concatMap = concat .$ map
]> sum23 = (+) . (2*) $. (3*)  -- \x y -> 2*x + 3*y

<br>
Here is another family of composition operators:

> (f $. g) x = (f) (g x)    -- a.k.a. (.)
> (f .$ g) x = (f x) (g)    -- a.k.a. flip
> (f $.. g) x y = (f) (g x y)
> (f .$. g) x y = (f x) (g y)
> (f ..$ g) x y = (f x y) (g)
> (f $... g) x y z = (f) (g x y z)
> (f .$.. g) x y z = (f x) (g y z)
> (f ..$. g) x y z = (f x y) (g z)
> (f ...$ g) x y z = (f x y z) (g)
> -- etc.
> infixl 8 $., .$, $..,.$.,..$, $...,.$..,..$.,...$


Think of the @.@ as the placeholder for an argument. It would be better if I could use @_@, but Haskell doesn't allow that. You can also think of the dots as the points from point-free style, so these operators allow for the preservation of the number of points :). With these operators the previous example becomes:

]> concatMap = concat $.. map
]> sum23 = (+) $. (2*) .$. (3*)  -- \x y -> 2*x + 3*y

I like the second family better, because they do not use @(.)@, which makes the first family more confusing.
What do you think? Would these operators be useful in practice?

