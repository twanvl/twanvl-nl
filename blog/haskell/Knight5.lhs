title: Knight in n, part 5: division
published: false

== Division ==

Polynomial long division

--> instance (Ix i, Num i, Num a) => Integral (Array i a) where
-->     divMod
-->     toInteger = error "Why should I implement toInteger if I want divMod?"

> divArray a b = listArray (bounds a - bounds b)
>                (divList (elems a) (elems b))

> divList []     ys     = []
> divList (x:xs) (y:ys) = multiplier : divList (subtract xs ys) (y:ys)
>   where
>     multiplier = x `div` y
>     subtract [] _  = []
>     subtract xs [] = xs
>     subtract (x:xs) (y:ys) = x - multiplier*y : subtract xs ys


]> test $ \x y -> x == (x * y) `divArray` y
