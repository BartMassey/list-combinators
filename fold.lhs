Copyright © 2011 Bart Massey, Jamey Sharp and Jules Kongslie

This work is licensed under the "MIT License". Please see
the file COPYING in the source distribution of this work for
license terms.

Generalized fold, with examples.

This fold generalizes a number of things from
'Data.List', including 'foldl' and 'foldr'. It works by
allowing `f` to work with both state accumulated from the
left and state built up from the right
simultaneously. 

'fold' is fully lazy if `f` is fully lazy
on `l` and `r`, strict if at most one of `l` and `r` is
strict, and is bottom if both `l` and `r` are strict.

One can think of 'fold' as processing each element of its
list input with a function that receives left context
calculated from its predecessors and a right context
calculated from its successors. As one traverses the list
and examines these elements, the function is run to produce
these outputs.

There is probably a need for versions of these functions
strict in the left context: call it fold' .

Compare this work with the "bifold" discussed a while back
on Haskell-Cafe:

    http://haskell.1045720.n5.nabble.com/Bifold-a-simultaneous-foldr-and-foldl-td3285581.html

That fold is identical to this one (up to trivial signature
differences). However, I think the subsumption results here
are new.  There is some interesting discussion of "Q" from
Backus that I would like to absorb someday.

| Given a function that accepts a left and right context and
an element and produces a new left and right context, an
initial left and right context, and a list, run the function
on each element of the list with the appropriate context.

> fold :: ((l, r) -> x -> (l, r)) -> (l, r) -> [x] -> (l, r)
> fold _ lr [] = lr
> fold f (l, r) (x : xs) =
>   let (l1, r1) = f (l, r2) x
>       (l2, r2) = fold f (l1, r) xs in
>   (l2, r1)

One can implement 'foldl' by just ignoring the right
component.

> foldl_ :: (a -> b -> a) -> a -> [b] -> a
> foldl_ f a0 = 
>   fst . fold f' (a0, undefined)
>   where
>     f' (l, r) x = (f l x, r)

One can implement 'foldr' by just ignoring the left
component.

> foldr_ :: (a -> b -> b) -> b -> [a] -> b
> foldr_ f b0 = 
>   snd . fold f' (undefined, b0)
>   where
>     f' (l, r) x = (l, f x r)

'mapAccumL' uses both left and right context in an
interesting way. It was actually the motivating example
for 'fold'.

> mapAccumL_ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
> mapAccumL_ f a0 =
>   fold f' (a0, [])
>   where
>     f' (l, rs) x =
>       let (l', r') = f l x in
>       (l', r' : rs)

'mapAccumR' can be written as a 'fold', but this is
boring; see immediately below.

> mapAccumR_ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
> mapAccumR_ f a0 =
>   snd . fold f' (undefined, (a0, []))
>   where
>     f' (l, (a, rs)) x =
>       let (a', r') = f a x in
>       (l, (a', r' : rs))

'mapAccumR' can be written as a 'foldr'. I have no idea
why the Haskell Standard Library does not implement it
this way. Then again, I have no idea why this function is
in the Haskell Standard Library at all.

> mapAccumR__ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
> mapAccumR__ f a0 =
>   foldr f' (a0, [])
>   where
>     f' x (a, rs) =
>       let (a', r') = f a x in
>       (a', r' : rs)

'scanl' can be written as a 'fold'.

> scanl_ :: (a -> b -> a) -> a -> [b] -> [a]
> scanl_ f z0 =
>   (z0 :) . snd . fold f' (z0, [])
>   where
>     f' (z, rs) x =
>       let z' = z `f` x in
>       (z', z' : rs)

'scanr' can be written as a 'foldr'.

> scanr__ :: (a -> b -> b) -> b -> [a] -> [b]
> scanr__ f z0 =
>   snd . foldr f' (z0, [z0])
>   where
>     f' x (z, rs) =
>       let z' = x `f` z in
>       (z', z' : rs)

'nub' can be written as a 'fold' using 'elem'.

> nub_ :: Eq a => [a] -> [a]
> nub_ =
>   snd . fold nub1 ([], [])
>   where
>     nub1 (l, rs) x
>       | x `elem` l = (l, rs)
>       | otherwise = (x : l, x : rs)

'take' can be written as a 'fold'.

> take_ :: Int -> [a] -> [a]
> take_ n0 =
>   snd . fold take1 (n0, [])
>   where
>     take1 (0, _) _ = (undefined, [])
>     take1 (n, rs) r | n > 0 = (n - 1, r : rs)
>     take1 _ _ = error "take with negative count"

'drop' can be written as a 'fold'.

> drop_ :: Int -> [a] -> [a]
> drop_ n0 =
>   snd . fold drop1 (n0, [])
>   where
>     drop1 (0, rs) r = (0, r : rs)
>     drop1 (n, rs) _ | n > 0 = (n - 1, rs)
>     drop1 _ _ = error "drop with negative count"
