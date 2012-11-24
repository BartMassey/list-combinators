-- Copyright © 2012 Bart Massey
-- [This program is licensed under the "BSD License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Combinator
-- Copyright   :  (c) 2012 Bart Massey
-- License     :  BSD-style (see the file COPYING)
-- 
-- Maintainer  :  bart.massey@gmail.com
-- Stability   :  pre-alpha
-- Portability :  portable
--
-- This re-implementation of 'Data.List' was inspired 
-- by working with Bart Massey, Jamey Sharp and Jules
-- Kongslie's generalized 'fold'.  
--
-- The documentation and a bit of the implementation are
-- either taken from or inspired by the 'base' 'Data.List'
-- implementaiton of GHC. However, this is not a lightly-hacked 
-- 'Data.List:
-- 
--   * The documentation has been substantially rewritten to
--   provide a more accurate description
--   
--   * The primitives have been almost entirely
--   reimplemented, with the goal of eliminating as many
--   recursive constructions as possible.  Most primitives
--   are now implemented in terms of each other; the
--   remaining few are implemented in terms of generalized
--   'fold' and 'unfold' operations.
-- 
--   * The handling of primitives involving numbers has
--   changed to use 'Integer' rather than 'Int' as the
--   default type. This future-proofs the library and
--   makes the Laws of these operations easier to use,
--   presumably at the expense of performance.
--   
--   * A few new primitives have been added.
--
-- The result of this work is a library that is likely to be
-- less performant than the GHC 'base' library. However, it
-- is hopefully easier to understand, verify and maintain.
-- 
-- Every function described here is maximally productive,
-- unless otherwise specified. Informally, this means that
-- the result of the function will be built up in such a way
-- that each piece will be ready for consumption as early as
-- logically possible.
-----------------------------------------------------------------------------


module Data.List.Combinator (
  module Prelude,
  -- * Basic Functions
  (++),
  head,
  last,
  tail,
  init,
  null,
  length,
  length',
  -- * List Transformations
  map,
  reverse,
  intersperse,
  intercalate,
  transpose,
  subsequences,
  permutations,
  insertions,
  -- * Reducing Lists (Folding)
  fold,
  foldl,
  foldl',
  foldr,
  foldl1,
  foldl1',
  foldr1,
  -- * Special Folds
  concat,
  concatMap,
  and,
  or,
  any,
  all,
  sum,
  sum',
  product,
  product',
  maximum,
  minimum,
  -- * Building Lists
  -- ** Scans
  scanl,
  scanl1,
  scanr,
  scanr1,
  -- ** Accumulating Maps
  mapAccumL,
  mapAccumR,
  -- ** Infinite Lists
  iterate,
  repeat,
  replicate,
  cycle,
  -- ** Unfolding
  unfold,
  unfoldr,
  unfoldl,
  -- * Sublists
  -- ** Extracting Sublists
  take,
  drop,
  splitAt,
  splits,
  takeWhile,
  dropWhile,
  span,
  break,
  takeWhileEnd,
  dropWhileEnd,
  spanEnd,
  breakEnd,
  stripPrefix,
  stripSuffix,
  group,
  inits,
  tails,
  -- ** Predicates
  isPrefixOf,
  isSuffixOf,
  isInfixOf,
  -- * Searching Lists
  -- ** Searching By Equality
  elem,
  notElem,
  lookup,
  -- ** Searching With A Predicate
  find,
  filter,
  partition,
  elemIndex,
  elemIndices,
  findIndex,
  findIndices,
  -- * Indexing Lists
  (!!),
  zip,
  zip3,
  zip4,
  zipWith,
  zipWith3,
  zipWith4,
  unzip,
  unzip3,
  unzip4,
  lines,
  words,
  unlines,
  unwords,
  nub,
  delete,
  listDiff,
  listDiff',
  union,
  union',
  intersect,
  intersect',
  merge,
  sort,
  insert,
  insert',
  elemBy,
  notElemBy,
  nubBy,
  deleteBy,
  listDiffBy,
  listDiffBy',
  unionBy,
  unionBy',
  intersectBy,
  intersectBy',
  groupBy,
  mergeBy,
  sortBy,
  insertBy,
  insertBy',
  maximumBy,
  minimumBy,
  stripPrefixBy,
  stripSuffixBy,
  isPrefixOfBy,
  isSuffixOfBy,
  isInfixOfBy,
  lookupBy,
  elemIndexBy,
  elemIndicesBy,
  genericLength ) where

import Prelude hiding (
  (++),
  foldl,
  foldr,
  head,
  last,
  tail,
  init,
  null,
  length,
  map,
  reverse,
  foldl1,
  foldr1,
  concat,
  concatMap,
  and,
  or,
  any,
  all,
  sum,
  product,
  maximum,
  minimum,
  scanl,
  scanl1,
  scanr,
  scanr1,
  iterate,
  repeat,
  replicate,
  cycle,
  take,
  drop,
  splitAt,
  takeWhile,
  dropWhile,
  span,
  break,
  elem,
  notElem,
  lookup,
  filter,
  (!!),
  zip,
  zip3,
  zipWith,
  zipWith3,
  unzip,
  unzip3,
  lines,
  words,
  unlines,
  unwords )
import Data.Char (isSpace)


-- XXX Need to write laws for this.

-- | Given a function that accepts an element and a left and
-- right context and produces a new left and right context,
-- and given an initial left and right context and a list,
-- run the function on each element of the list with the
-- appropriate context.
-- 
-- The 'fold' operation generalizes a
-- number of things from 'Data.List', including 'foldl' and
-- 'foldr'. It works by allowing `f` to work with both state
-- accumulated from the left and state built up from the
-- right simultaneously.
-- 
-- @'fold' f (l, r)@ is fully lazy if `f` is fully lazy
-- on `l` and `r`, strict if at most one of `l` and `r` is
-- strict, and is bottom if both `l` and `r` are strict.
-- 
-- One can think of 'fold' as processing each element of its
-- list input with a function that receives left context
-- calculated from its predecessors and a right context
-- calculated from its successors. As one traverses the list
-- and examines these elements, the function is run to produce
-- these outputs.
-- 
-- There is probably a need for versions of these functions
-- strict in the left context: call it 'fold'' .
-- 
-- Compare this 'fold' with Noah Easterly's "bifold" discussed
-- a while back on Haskell-Cafe
-- (<http://haskell.1045720.n5.nabble.com/Bifold-a-simultaneous-foldr-and-foldl-td3285581.html>). That
-- fold is identical to this one (up to trivial signature
-- differences). (I do not understand whether Henning
-- Theielemann's "foldl'r" is the same. There is some
-- interesting discussion of "Q" from Backus in that thread
-- that I would like to absorb someday.)  In any case, I
-- think there is enough novelty in the use of 'fold' in
-- this library to be worth paying attention to. /O(n)/ plus
-- the cost of evaluating the folding function.
fold :: (x -> (l, r) -> (l, r)) -> (l, r) -> [x] -> (l, r)
fold f lr0 xs0 =
  g lr0 xs0
  where
    g lr [] = lr
    g (l, r) (x : xs) =
      let (l1, r1) = f x (l, r2)
          (l2, r2) = g (l1, r) xs  in
      (l2, r1)

-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
--
-- /O(m)/. Laws:
-- 
-- > forall xs . xs ++ [] == [] ++ xs == xs
-- > forall x xs ys . x : (xs ++ ys) == (x : xs) ++ ys
-- > forall xs ys zs . (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

-- | Return the first element of a non-empty
-- list. /O(1)/. Laws:
-- 
-- > forall l : List a | not (null l) . (head l : tail l) == l
head :: [a] -> a
head (x : _) = x

-- | Return the last element of a non-empty
-- list. Strict. /O(1)/. Laws:
-- 
-- > forall l | (exists k : Integer . k > 1 && length l == k) . 
-- >   init l ++ [tail l] == l
last :: [a] -> a
last (x : xs) = foldl (\_ y -> y) x xs

-- | Return the second and subsequent elements of a
-- non-empty list. /O(1)/. Laws:
-- 
-- > forall l : List a | not (null l) . (head l : tail l) == l
tail :: [a] -> [a]
tail (_ : xs) = xs

-- | Return all the elements of a non-empty list except the
-- last one. /O(1)/. Laws:
-- 
-- > forall l | (exists k : Integer . k > 1 && length l == k) . 
-- >   init l ++ [tail l] == l
init :: [a] -> [a]
init xs0 =
  foldr f [] xs0
  where
    f x [] = []
    f x as = (x : as)

-- | Return 'True' on the empty list, and 'False'
-- on any other list. /O(1)/. Laws:
-- 
-- > null [] == True
-- > forall x xs . null (x : xs) == False
null :: [a] -> Bool
null [] = True
null  _ = False

-- | Returns the length of a finite list as an 'Integer'. See also
-- 'genericLength'. Strict. /O(n)/. Laws:
-- 
-- > length [] == 0
-- > forall x xs | (exists k . length xs <= k) . 
-- >   length (x : xs) == 1 + length xs
length :: [a] -> Integer
length xs = genericLength xs

-- | Returns the length of a list with less than or
-- equal to 'maxBound' 'Int' elements as an 'Int'. See also
-- 'genericLength'. Strict. /O(n)/. Laws: 
-- 
-- > length' [] == 0
-- > forall x xs | length xs < maxBound :: Int .
-- >   length' (x : xs) == 1 + length' xs
length' :: [a] -> Int
length' xs = genericLength xs

-- | @'map' f xs@ applies @f@ to each element
-- of @xs@ in turn and returns a list of the results, i.e.,
-- 
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- 
-- /O(n)/ plus the cost of the function applications. Laws:
-- 
-- > forall f . map f [] == []
-- > forall f x xs . map f (x : xs) = f x : map f xs
map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x a -> f x : a) [] xs

-- | 'reverse' returns the elements of its input list in
-- reverse order. Strict. /O(n)/. Laws:
-- 
-- > reverse [] == []
-- > forall xs | not (null xs) . 
-- >   reverse xs = last xs : reverse (init xs)
reverse :: [a] -> [a]
reverse xs = foldl' (\a x -> x : a) [] xs

-- | The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
-- 
-- > intersperse ',' "abcde" == "a,b,c,d,e"
-- 
-- /O(n)/. Laws:
-- 
-- > forall t . intersperse t [] == []
-- > forall t x . intersperse t [x] == [x]
-- > forall t x xs | not (null xs) . 
-- >   intersperse t (x : xs) == x : intersperse t xs
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse s xs = tail $ foldr (\x a -> s : x : a) [] xs

-- This intercalate is taken directly from Data.List.

-- | @'intercalate' xs xss@ inserts the list @xs@ in between
-- the lists in @xss@ and returns the concatenation of
-- resulting lists. /O(n)/. Laws:
-- 
-- > forall xs xss .
-- >   intercalate xs xss == concat (intersperse xs xss)
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- | The 'transpose' function transposes the rows and columns of its argument.
-- For example,
--
-- >>> transpose [["a1","b1"],["a2","b2","c2"],["a3","b3"]]
-- [["a1","a2","a3"],["b1","b2","b3"],["c2"]]
-- 
-- /O(n)/ where /n/ is the number of elements to be
-- transposed. Laws:
-- 
-- > forall xss yss | yss == filter (not . null) xss && null yss . 
-- >   transpose xss == []
-- > forall xss yss | yss == filter (not . null) xss && not (null yss) . 
-- >   transpose xss == map head yss : transpose (map tail yss)
transpose :: [[a]] -> [[a]]
transpose xss =
  unfoldr f xss
  where
    f a 
      | null xs  = Nothing
      | otherwise =
          Just (foldr g ([], []) xs)
      where
        xs = filter (not . null) a
        g (y : ys) (h, t) = (y : h, ys : t)

-- | The 'subsequences' function returns the list of all
-- subsequences (ordered sublists) of its argument, in no
-- specified order. /O(2^n)/. Laws:
-- 
-- > subsequences [] == [[]]
-- > forall x xs . subsequences (x : xs) `elem` 
-- >   permutations (subsequences xs ++ map (x :) (subsequences xs))
subsequences :: [a] -> [[a]]
subsequences xs =
  foldr f [[]] xs
  where
    f x a = a ++ (map (x :) a)

-- | The 'permutations' function returns the list of lists
-- of all permutations of its list argument, in no specified
-- order. /O(n!)/. Laws:
-- 
-- > permutations [] == [[]]
-- > forall x xs . permutations (x : xs) `elem`
-- >   permutations (concatMap (insertions x) (permutations xs))
permutations :: [a] -> [[a]]
permutations xs =
  foldr f [[]] xs
  where
    f x a = concatMap (insertions x) a

-- | @'insertions' t xs@ returns a list of the lists
-- obtained by inserting @t@ at every position in @xs@ in
-- sequence, in order from left to
-- right. [New.] /O(n^2)/. Laws:
-- 
-- > forall t . insertions t [] == [[t]]
-- > forall t x xs . 
-- >   insertions t (x : xs) == [t : x : xs] : map (x :) (insertions t xs)
insertions :: a -> [a] -> [[a]]
insertions x xs =
  snd $ foldr f ([], [[x]]) xs
  where
    f y ~(l, r) = (y : l, (x : y : l) : map (y :) r)

-- | 'foldl', applied to a binary operator @f@, a starting value (typically
-- the left-identity of the operator) @a@, and a list, reduces the list
-- using @f@, from left to right:
-- 
-- > foldl f a [x1, x2, ..., xn] == (...((a `f` x1) `f` x2) `f`...) `f` xn
-- 
-- The starting value @a@ is best thought of as an \"accumulator\" that
-- is passed from the beginning to the end of the list and then returned.
-- 
-- Strict. /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f a . foldl f a [] == a
-- > forall a x xs . foldl f a (x : xs) == foldl f (f a x) xs
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = (f l x, r)

-- | The semantics of 'foldl'' those of 'foldl', except that
-- 'foldl'' eagerly applies @f@ at each step. This gains
-- some operational efficiency, but makes 'foldl'' only
-- partially correct: there are cases where 'foldl' would
-- return a value, but 'foldl'' on the same arguments will
-- nonterminate or fail. The Laws for 'foldl'' are thus
-- the same as those for 'foldl' up to a condition on @f@,
-- which is not given here.
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a0 xs0 =
  g a0 xs0
  where
    g a [] = a
    g a (x : xs) =
      (g $! ((f $! a) $! x)) $! xs

-- XXX The strictness isn't right here currently, maybe?
foldl'_0 :: (a -> b -> a) -> a -> [b] -> a
foldl'_0 f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = ((f $! l) $! x, r)

-- | 'foldl1' is a variant of 'foldl' that 
-- starts the fold with the accumulator set
-- to the first element of the list; this
-- requires that its list argument be nonempty.
-- Spine-strict. /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f x xs . foldl1 f (x : xs) == foldl f x xs
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs 

-- | 'foldl1'' bears the same relation to 'foldl1' that
-- 'foldl'' bears to 'foldl'.
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x : xs) = foldl' f x xs 

-- | 'foldr', applied to a binary operator @f@, a starting value (typically
-- the right-identity of the operator) @a@, and a list, reduces the list
-- using @f@, from right to left:
-- 
-- > foldr f a [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` a)...)
-- 
-- The starting value @a@ is best thought of as an
-- \"accumulator\" that is returned from processing the last
-- element of the list and processed by subsequent returns,
-- finally being returned as the result. Because 'foldr' is
-- lazy, and proceeds (of necessity) by calls from left to
-- right on the list, it is possible for 'foldr' to \"stop
-- processing early\" if @f@ simply returns its accumulator
-- @a@. /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f a . foldr f a [] == a
-- > forall f a x xs . foldr f a (x : xs) == f x (foldr f a xs)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b0 =
  snd . fold f' (undefined, b0)
  where
    f' x (l, r) = (l, f x r)

-- | 'foldr1' is a variant of 'foldr' that 
-- folds with the accumulator set
-- to the last element of the list; this
-- requires that its list argument be nonempty.
-- /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f xs x . foldr1 f (xs ++ [x]) == foldr f x xs
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f xs =
  let Just a0 = foldr f' Nothing xs in a0
  where
    f' x Nothing = Just x
    f' x (Just a) = Just (f x a)

-- | Given a list of lists, \"concatenate\" the lists, that
-- is, append them all together to make a list of the
-- elements. /O(n)/. Laws:
-- 
-- > concat [] == []
-- > forall xs xss . concat (xs : xss) == xs ++ concat xss
concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

-- | Apply 'concat' to the result of 'map'-ing a function
-- @f@ onto a list; the result of @f@ must of necessity be a
-- list.  A convenience function, and may be more efficient
-- by a constant factor than the obvious
-- implementation. /O(n)/ plus the cost of evaluating
-- @f@. Laws:
-- 
-- > forall f xs . concatMap f xs == concat (map f xs)
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs =
  foldr (\x a -> f x ++ a) [] xs

-- | The 'and' function returns the conjunction of the
-- elements of its 'Boolean' list argument. This is
-- \"short-circuiting\" 'and': it scans the list from left
-- to right, returning 'False' the first time a 'False'
-- element is encountered. Thus, it is strict in the case
-- the list is entirely 'True', and spine-lazy
-- otherwise. /O(n)/. Laws:
-- 
-- and [] == True
-- forall x xs . and (x : xs) == x && and xs
and :: [Bool] -> Bool
and = foldr (&&) True

-- | The 'or' function returns the disjunction of the
-- elements of its 'Boolean' list argument. This is
-- \"short-circuiting\" 'or': it scans the list from left
-- to right, returning 'True' the first time a 'True'
-- element is encountered. Thus, it is strict in the case
-- the list is entirely 'False', and spine-lazy
-- otherwise. /O(n)/. Laws:
-- 
-- or [] == False
-- forall x xs . or (x : xs) == x || or xs
or :: [Bool] -> Bool
or = foldr (||) False

-- | The 'any' function returns 'True' if its predicate
-- argument @p@ applied to any element of its list argument
-- returns 'True', and returns 'False' otherwise. This is
-- \"short-circuiting\" any, as with 'or'. /O(n)/ plus the
-- cost of evaluating @p@. Laws:
-- 
-- > forall p xs . any p xs == or (map p xs)
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- | The 'all' function returns 'False' if its predicate
-- argument @p@ applied to any element of its list argument
-- returns 'False', and returns 'True' otherwise. This is
-- \"short-circuiting\" any, as with 'and'. /O(n)/ plus the
-- cost of evaluating @p@. Laws:
-- 
-- > forall p xs . all p xs == and (map p xs)
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- | Returns the sum of a list of numeric arguments. The
-- sum of the empty list is defined to be @0@.
-- [New. See 'sum'' below.] Strict. /O(n)/. Laws:
-- 
-- sum [] == 0
-- forall x xs . sum (x : xs) == x + sum xs
sum :: Num a => [a] -> a
sum = 
  foldl' plus 0
  where
    a `plus` b = ((+) $! a) $! b


-- | Returns the sum of a list of numeric arguments. The
-- sum of the empty list is defined to be @0@.
-- 
-- This should be element-strict (like foldl'), but this is
-- not allowed by the Standard (according to the GHC folks),
-- because it is possible that some kind of non-standard
-- non-strict \"numbers\" could be implemented as a 'Num'
-- instance. Thus, with standard numbers, this sum will
-- not be executed at all efficiently: the result will
-- be a function that, when evaluated, will produce a sum.
-- To be fair, compiler strictness analysis will usually
-- fix this problem: you will normally only see it when
-- the compiler fails to optimize.
-- 
-- /O(n)/. Laws:
-- 
-- sum' [] == 0
-- forall x xs . sum' (x : xs) == x + sum' xs
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

-- | Returns the product of a list of numeric arguments. The
-- product of the empty list is defined to be @1@. This
-- version will terminate early if a numeric @0@ is
-- encountered in the list, and thus will work on infinite
-- lists containing @0@ and will be more efficient on lists
-- containing @0@. [New. See 'product'' below.]
-- Strict, up to handling @0@. /O(n)/. Laws:
-- 
-- > product [] == 1
-- > forall x xs . product (x : xs) == x * product xs
product :: (Num a, Eq a) => [a] -> a
product = 
  foldr times 1
  where
    0 `times` _ = 0
    _ `times` 0 = 0
    x `times` y = ((*) $! x) $! y

-- | Returns the product of a list of numeric arguments. The
-- product of the empty list is defined to be @1@.
-- 
-- Because standard numeric (*) is
-- strict, this will not terminate early as expected when taking
-- products with standard numeric @0@. Nonetheless, as with 'sum'',
-- this product is apparently required to be element-lazy.
-- 
-- /O(n)/. Laws:
-- 
-- > product' [] == 1
-- > forall x xs . product' (x : xs) == x * product' xs
product' :: Num a => [a] -> a
product' = foldl (*) 1

-- | The 'maximum' function returns a maximum value from a
-- non-empty list of 'Ord' elements by way of the 'max'
-- function. If there are multiple maxima, the choice of
-- which to return is unspecified. See also
-- 'maximumBy'. Strict. /O(n)/. Laws:
-- 
-- > forall x . maximum [x] == x
-- > forall x xs | not (null xs) .
-- >   maximum (x : xs) == x `max` maximum xs
maximum :: Ord a => [a] -> a
maximum = foldl1' max

-- | The 'minimum' function returns a minimum value from a
-- non-empty list of 'Ord' elements by way of the 'min'
-- function. If there are multiple minima, the choice of
-- which to return is unspecified. See also
-- 'minimumBy'. Strict. /O(n)/. Laws:
-- 
-- > forall x . minimum [x] == x
-- > forall x xs | not (null xs) .
-- >   minimum (x : xs) == x `min` maximum xs
minimum :: Ord a => [a] -> a
minimum = foldl1' min

-- | The 'scanl' function is similar to 'foldl', 
-- in that 'scanl' passes an accumulator from left to right.
-- However, 'scanl' returns a list of successive accumulator values:
-- 
-- > scanl f a [x1, x2, ...] == [a, a `f` x1, (a `f` x1) `f` x2, ...]
-- 
-- Spine-strict. /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f a . scanl f a [] == [a]
-- > forall f a x xs . scanl f a (x : xs) == a : scanl f (f a x) xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f a0 =
  (a0 :) . snd . mapAccumL g a0
  where
    g a x = let a' = f a x in (a', a')

-- | The 'scanl1' function is to 'scanl' as 'foldl1' is to 'foldl'.
-- Spine-strict. /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f x xs . scanl1 f (x : xs) == scanl f x xs
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs

-- | The 'scanr' function is similar to 'foldr', in that
-- 'scanr' passes an accumulator from right to left.
-- However, 'scanr' returns a list of accumulator values (in
-- the order consistent with the
-- definition). Spine-strict. /O(n)/ plus the cost of
-- evaluating @f@. Laws:
-- 
-- > forall f a . scanr f a [] == [a]
-- > forall f a b x xs | bs == scanr f a xs . 
-- >   scanr f a (x : xs) == f x (head bs) : bs
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f a0 xs =
  foldr f' [a0] xs
  where
    f' x as@(a : _) = f x a : as

-- | The 'scanr1' function is to 'scanr' as 'foldr1' is to 'foldr'.
-- /O(n)/ plus the cost of evaluating @f@. Laws:
-- 
-- > forall f x xs . scanr1 f (xs ++ [x]) == scanr f x xs
scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f xs =
  foldr f' [] xs
  where
    f' x [] = [x]
    f' x as@(a : _) = f x a : as

-- | The 'mapAccumL' function behaves like a combination of
-- 'map' and 'foldl'; it applies a function to each element
-- of a list, passing an accumulating parameter from left to
-- right, and returns a final value of this accumulator
-- together with the new list. /O(n)/ plus the cost
-- of evaluating the folding function. Laws:
-- 
-- > forall f a . mapAccumL f a [] == (a, [])
-- > forall f a a' a'' x x' xs xs''
-- >   | (a', x') == f a x && (a'', xs'') == mapAccumL f a' xs .
-- >     mapAccumL f a (x : xs) == (a'', x' : xs'')
mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f a0 =
  fold f' (a0, [])
  where
    f' x ~(l, rs) =
      let ~(l', r') = f l x in
      (l', r' : rs)

-- | The 'mapAccumR' function behaves like a combination of
-- 'map' (which is essentially a 'foldr') and (another)
-- 'foldr'; it applies a function to each element of a list,
-- passing an accumulating parameter from right to left, and
-- returns a final value of this accumulator together with
-- the new list. 
-- 
-- Watch out: the folding function takes its
-- arguments in the opposite order of 'foldr'. Given that
-- 'mapAccumR' is essentially a 'foldr' on a pair, it
-- probably should not exist. 
-- 
-- /O(n)/ plus the cost of evaluating the folding
-- function. Laws:
-- 
-- > forall f a xs . mapAccumR f a [] == (a, [])
-- > forall f a a' a'' x x'' xs xs''
-- >   | (a'', x'') == f a' x && (a', xs') == mapAccumR f a xs .
-- >     mapAccumR f a (x : xs) == (a'', x'' : xs')
mapAccumR :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f a0 =
  foldr f' (a0, [])
  where
    f' x (a, rs) =
      let (a', r') = f a x in
      (a', r' : rs)

-- | @'iterate' f x@ returns an infinite list of repeated
-- applications of @f@ to @x@:
--     
-- > iterate f x == [x, f x, f (f x), ...]
-- 
-- Time complexity of this is almost entirely dependent on the
-- cost of evaluating @f@. Laws:
-- 
-- > forall f x . iterate f x == x : iterate f (f x)
iterate :: (a -> a) -> a -> [a]
iterate f x0 =
  unfoldr g x0
  where
    g x = Just (x, f x)

-- | @'repeat' x@ is an infinite list, with @x@ the value of
-- every element. /O(1)/. Laws:
-- 
-- > forall x . repeat x == x : repeat x
repeat :: a -> [a]
repeat x = cycle [x]

-- | Given a count @n@ and an element @e@, produce a list of
-- @e@s of length @n@.  This function is equivalent to
-- 'Data.List.genericReplicate'. /O(n)/. Laws:
-- 
-- > forall n e . replicate n e == take n (repeat e)
replicate :: Integral b => b -> a -> [a]
replicate n = take n . repeat

-- | Given an input list @xs@, return a
-- list consisting of an infinite repetition of
-- the elements of @xs@. /O(1)/ due to data recursion. Laws:
-- 
-- > forall xs . cycle xs == concat (repeat xs)
cycle :: [a] -> [a]
cycle xs =
  let ys = xs ++ ys in ys

-- XXX This generalized fold may be enough to write fold. I don't know.
-- XXX Need to write laws for this.

-- | This abstraction of 'fold' is a bidirectional
-- generalization of 'unfoldr'. Given an initial left and
-- right accumulator and a function that steps the
-- accumulators forward from the left and right as with
-- 'fold', keep stepping until the function indicates
-- completion by returning 'Nothing' on the right. /O(n)/
-- where /n/ is the number of unfolding steps, plus the cost
-- of evaluating the folding function.
unfold :: ((l, r) -> (l, Maybe r)) -> (l, r) -> (l, r)
unfold f (l, r) =
  let (l1, mr1) = f (l, r2)
      (l2, r2) = unfold f (l1, r) in
  case mr1 of
    Nothing -> (l1, r)
    Just r1 -> (l2, r1)

-- XXX Need to write laws for this.

-- | The 'unfoldr' function is a \"dual\" to 'foldr': while
-- 'foldr' reduces a list to a summary value, 'unfoldr'
-- builds a list from a seed value.  The function takes the
-- current accumulator and returns 'Nothing' if it is done
-- producing the list. Otherwise, it returns @'Just' (x, a)@,
-- in which case @x@ is a prepended to the list and @a@ is
-- used as the next accumulator in a recursive call.  For
-- example,
--
-- > iterate f == unfoldr (\x -> Just (x, f x))
--
-- In some cases 'unfoldr' can undo a 'foldr' operation:
--
-- > unfoldr f' (foldr f a0 xs) == xs
--
-- This works if the following hold:
--
-- > f' (f x a) = Just (x, a)
-- > f' a0      = Nothing
--
-- A simple use of unfoldr:
--
-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- >  [10,9,8,7,6,5,4,3,2,1]
--
-- /O(n)/ plus the cost of evaluating @f@, where /n/ is the
-- length of the produced list.
unfoldr :: (a -> Maybe (b, a)) -> a -> [b]
unfoldr f a0 =
  snd $ unfold g (a0, [])
  where
    g (a, xs) =
      case f a of
        Nothing -> 
          (a, Nothing)
        Just (x, a') ->
          (a', Just (x : xs))

-- | An unfoldl is provided for completeness. [New.]
-- Spine-strict. /O(n)/ plus the cost of evaluating the
-- unfolding function. Laws:
-- 
-- > forall f a . unfoldl f a == reverse (unfoldr (fmap swap . f) a)
unfoldl :: (a -> Maybe (a, b)) -> a -> [b]
unfoldl f a0 =
  snd $ fst $ unfold g ((a0, []), ())
  where
    g ((a, xs), _) =
      case f a of
        Nothing -> 
          ((a, xs), Nothing)
        Just (a', x) ->
          ((a', x : xs), Just ())

-- | An unfoldl' is provided for the same reasons as
-- 'foldl''.  Element-strict (but not spine-strict). /O(n)/
-- plus the cost of evaluating the unfolding function.
unfoldl' :: (a -> Maybe (a, b)) -> a -> [b]
unfoldl' f a0 =
  snd $ fst $ unfold g ((a0, []), ())
  where
    g ((a, xs), _) =
      case f a of
        Nothing -> 
          ((a `seq` a, xs), Nothing)
        Just (a', x) ->
          ((a' `seq` a', x : xs), Just ())


-- | @'take' n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@.
-- Some examples:
-- 
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
-- 
-- This function is equivalent to
-- 'Data.List.genericTake'. /O(n)/ where /n/ is
-- the number of elements taken. Laws:
-- 
-- > forall n xs | n <= 0 . take n xs == []
-- > forall n | n >= 0 . take n [] == []
-- > forall n (x : xs) | n > 0 . 
-- >   take n (x : xs) == x : take (n - 1) xs
take :: Integral b => b -> [a] -> [a]
take n = fst . splitAt n

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@.
-- Some examples:
-- 
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
-- 
-- This function is equivalent to
-- 'Data.List.genericDrop'. /O(n)/.
-- Laws:
-- 
-- > forall n xs | n <= 0 . drop n xs == xs
-- > forall n | n >= 0 . drop n [] == []
-- > forall n (x : xs) | n > 0 . 
-- >   drop n (x : xs) == drop (n - 1) xs
drop :: Integral b => b -> [a] -> [a]
drop n = snd . splitAt n

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
-- 
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
-- 
-- @splitAt n xs@ is equivalent to @('take' n xs, 'drop' n xs)@. [This
-- is a generalization of 'Data.List.genericSplitAt' in that
-- the top-level tuple is always returned even if /n/ is bottom.]
-- /O(n)/. Laws:
-- 
-- > forall n xs . splitAt n xs = (take n xs, drop n xs)
splitAt :: Integral b => b -> [a] -> ([a], [a])
splitAt n0 xs =
  snd $ fold f (n0, ([], [])) xs
  where
    f x ~(n, ~(ts, ds))
      | n <= 0 = (n - 1, ([], x : ds))
      | otherwise = (n - 1, (x : ts, ds))

-- | Returns a list of all possible splits of its list
-- argument as produced by 'splitAt' in order of
-- increasing @n@. [New.] /O(n)/. Laws:
-- 
-- > forall n xs | length xs == n .
-- >   splits xs == zipWith splitAt [0..n] (repeat xs)
splits :: [a] -> [([a], [a])]
splits xs0 =
  snd $ fold f (0, [(xs0, [])]) xs0
  where
    f _ (n, xs) = (n + 1, splitAt n xs0 : xs)

-- | The 'takeWhile' function applied to a predicate @p@ and
-- a list @xs@ returns the longest prefix (possibly empty)
-- of @xs@ of elements that satisfy @p@. Some examples:
-- 
-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- > takeWhile (< 0) [1,2,3] == []
-- 
-- /O(n)/ plus the cost of evaluating @p@. Laws:
-- 
-- > forall p x xs | not (p x) . 
-- >   takeWhile p (x : xs) == []
-- > forall p x xs | p x . 
-- >   takeWhile p (x : xs) == x : takeWhile p xs
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = fst $ span p xs

-- | @'dropWhile' p xs@ returns the suffix remaining after
-- @'takeWhile' p xs@. Some examples:
-- 
-- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
-- > dropWhile (< 9) [1,2,3] == []
-- > dropWhile (< 0) [1,2,3] == [1,2,3]
-- 
-- /O(n)/ plus the cost of evaluating @p@. Laws:
-- 
-- > forall p x xs | not (p x) . 
-- >   dropWhile p (x : xs) == x : xs
-- > forall p x xs | p x . 
-- >   dropWhile p (x : xs) == dropWhile p xs
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = snd $ span p xs

-- Weird new list functions, but OK.

-- | The 'spanEnd' function returns a tuple whose second element
-- is the largest suffix of its list argument @xs@ such that
-- its predicate @p@ holds for every element; the first element
-- of the returned tuple is the remaining prefix of the list.
-- Some examples:
-- 
-- > spanEnd isSpace "foo bar \n" == ("foo bar", " \n")
-- > spanEnd isSpace "foo bar" == ("foo bar", "")
-- 
-- @'spanEnd' p xs@ is equivalent to 
-- @('takeWhileEnd' p xs, 'dropWhileEnd' p xs)@.
-- [New.] /O(n)/ plus the cost of evaluating the predicate. Laws:
-- 
-- forall p xs . spanEnd p xs == (takeWhileEnd p xs, dropWhileEnd p xs)
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs =
  snd $ foldr f (True, ([], [])) xs
  where
    f x ~(ok, (ts, ds))
      | p x && ok = (True, (ts, x : ds))
      | otherwise = (False, (x : ts, ds))

-- | The 'breakEnd' function returns a tuple whose second element
-- is the largest suffix of its list argument @xs@ such that
-- its predicate @p@ does not hold for any element; the first element
-- of the returned tuple is the remaining prefix of the list.
-- Some examples:
-- 
-- > breakEnd isSpace "foo bar" == ("foo ", "bar")
-- > breakEnd isSpace "foobar" == ("", "foobar")
-- 
-- @'breakEnd' p xs@ is equivalent to 
-- @'spanEnd' (not p) xs@.
-- [New.] /O(n)/ plus the cost of evaluating the predicate. Laws:
-- 
-- forall p xs . breakEnd p xs == spanEnd (not p) xs
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p xs = spanEnd (not . p) xs

-- | The 'takeWhileEnd' function returns the largest suffix of a list
-- in which the given predicate holds for all elements.  Some examples:
--
-- > takeWhileEnd isSpace "foo\n" == "\n"
-- > takeWhileEnd isSpace "foo bar" == ""
-- 
-- [New.] /O(n)/ plus the cost of evaluating the predicate. Laws:
-- 
-- > forall p x xs | not (p x) . 
-- >   takeWhileEnd p (xs ++ [x]) == []
-- > forall p x xs | p x . 
-- >   takeWhileEnd p (xs ++ [x]) == takeWhileEnd p xs ++ [x]
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p xs = snd $ spanEnd p xs

-- | The 'dropWhileEnd' function drops the largest suffix of a list
-- in which the given predicate holds for all elements.  Some examples:
--
-- > dropWhileEnd isSpace "foo\n" == "foo"
-- > dropWhileEnd isSpace "foo bar" == "foo bar"
-- > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
-- 
-- /O(n)/ plus the cost of evaluating the predicate. Laws:
-- 
-- > forall p x xs | not (p x) . 
-- >   dropWhileEnd p (xs ++ [x]) == xs ++ [x]
-- > forall p x xs | p x . 
-- >   dropWhileEnd p (xs ++ [x]) == dropWhileEnd p xs
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p xs = fst $ spanEnd p xs

-- | 'span', applied to a predicate @p@ and a list @xs@,
-- returns a tuple where the first element is the longest prefix
-- of @xs@ whose elements satisfy @p@ and
-- the second element is the remainder of the list.  Some
-- examples:
-- 
-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- > span (< 9) [1,2,3] == ([1,2,3],[])
-- > span (< 0) [1,2,3] == ([],[1,2,3])
-- 
-- @'span' p xs@ is equivalent to @('takeWhile' p xs,
-- 'dropWhile' p xs)@. /O(n)/ plus the cost of evaluating
-- @p@. Laws:
-- 
-- > forall p xs . span p xs == (takeWhile p xs, dropWhile p xs)
span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs =
  snd $ fold f (True, ([], [])) xs
  where
    f x (b, ~(l, r))
      | b && p x = (True, (x : l, r))
      | otherwise = (False, ([], x : r))

-- | 'break', applied to a predicate @p@ and a list @xs@,
-- returns a tuple where the first element is the longest prefix
-- of @xs@ whose elements that /do not
-- satisfy/ @p@ and the second element is the remainder of the
-- list. Some examples:
-- 
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
-- 
-- 'break' @p@ is equivalent to @'span' ('not' . p)@. 
-- /O(n)/. Laws:
-- 
-- > forall p xs . break p xs == span (not p) xs
break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

-- | The 'stripPrefix' function drops the given prefix from a list.
-- It returns 'Just' the suffix of the list after the given prefix
-- if the prefix matches, and 'Nothing' otherwise.
-- Some examples:
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix "foo" "foo" == Just ""
-- > stripPrefix "foo" "barfoo" == Nothing
-- > stripPrefix "foo" "barfoobaz" == Nothing
-- 
-- /O(n)/ where /n/ is the length of the prefix. This is a
-- special case of 'stripPrefixBy' with @(==)@ as the equality
-- predicate. Laws:
-- 
-- > forall ps xs . stripPrefix ps xs == stripPrefixBy (==) ps xs
stripPrefix  :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix ps xs = stripPrefixBy (==) ps xs

-- | The 'stripPrefixBy' function drops the given prefix from a list.
-- It returns 'Just' the suffix of the list after the given prefix
-- if the prefix matches, and 'Nothing' otherwise. A user-supplied
-- predicate is used for testing element equality.
-- 
-- /O(n)/ where /n/ is the length of the prefix.  Laws:
-- 
-- > forall eq xs . stripPrefixBy eq [] xs == Just xs
-- > forall eq . stripPrefixBy eq [] [] == Just []
-- > forall eq ps | not (null ps) . 
-- >   stripPrefixBy eq ps [] == Nothing
-- > forall eq p ps x xs | not (null ps) && not (null xs) && not (eq p x) . 
-- >   stripPrefixBy eq (p : ps) (x : xs) == Nothing
-- > forall eq p ps x xs | not (null ps) && not (null xs) && eq p x . 
-- >   stripPrefixBy eq (p : ps) (x : xs) == stripPrefixBy eq ps xs
stripPrefixBy  :: (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy p ps xs =
  foldl f (Just ps) xs
  where
    f Nothing _ = Nothing
    f (Just []) _ = Nothing
    f (Just (y : ys)) x
      | p x y = Just ys
      | otherwise = Nothing

-- | The 'stripSuffix' function drops the given suffix from a list.
-- It returns 'Just' the prefix of the list before the given suffix
-- if the suffix matches, and 'Nothing' otherwise. 'stripSuffix' is
-- a special case of 'stripBy' using '(==)' as the equality predicate.
-- Laws:
-- 
-- > forall ss xs . stripSuffix ss xs == stripSuffixBy (==) ss xs
stripSuffix  :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix = stripSuffixBy (==)

-- | The 'stripSuffixBy' function drops the given suffix from a list.
-- It returns 'Just' the prefix of the list before the given suffix
-- if the suffix matches, and 'Nothing' otherwise. A user-supplied
-- predicate is used for testing element equality.
-- 
-- [New.] /O(m n)/ where /m/ is the length of the prefix and /n/ is the
-- length of the list..  Laws:
-- 
-- > forall eq xs . stripSuffixBy eq [] xs == Just xs
-- > forall eq . stripSuffixBy eq [] [] == Just []
-- > forall eq ss | not (null ss) . 
-- >   stripSuffixBy eq ss [] == Nothing
-- > forall eq s ss x xs | not (null ps) && not (null xs) && not (eq s x) . 
-- >   stripSuffixBy eq (ss ++ [s]) (xs ++ [x]) == Nothing
-- > forall eq s ss x xs | not (null ss) && not (null ss) && eq s x . 
-- >   stripSuffixBy eq (ss ++ [s]) (xs ++ [x]) == stripSuffixBy eq ps xs
stripSuffixBy  :: (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripSuffixBy eq ps xs0 =
  lookupBy (`equals` ps) $ map (\(x, y) -> (y, x)) $ splits xs0
  where
    xs `equals` ys =
      case foldl f (True, xs) ys of
        (True, []) -> True
        _ -> False
      where
        f (_, []) _ = (False, [])
        f (ok, (y : ys)) x = (ok && eq x y, ys)

-- | The 'group' function takes a list and returns a
-- partition of that list such that elements @x1@ and @x2@
-- share a partition if and only if they are adjacent and
-- equal. Thus, each sublist in the resulting list of lists
-- contains only equal elements, and the concatenation of
-- the result is the original list.  For example:
-- 
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
-- 
-- This function is a special case of 'groupBy' using the '(==)' 
-- predicate. /O(n)/. Laws:
-- 
-- > forall xs . group xs == groupBy (==) xs
group :: Eq a => [a] -> [[a]]
group xs0 = groupBy (==) xs0

-- | The 'groupBy' function takes a list and returns a
-- partition of that list such that elements @x1@ and @x2@
-- share a partition if and only if they are adjacent and
-- they are equal according to the given equality
-- test. Thus, each sublist in the resulting list of lists
-- contains only equal elements, and the concatenation of
-- the result is the original list.  For example:
-- 
-- > groupBy (==) "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
-- 
-- /O(n)/. Laws:
-- 
-- > forall p . groupBy p [] == []
-- > forall p x xs xss | groupBy p xs == [] : xss .
-- >   groupBy p (x : xs) == [x] : xss
-- > forall p x xs x1 xs1 xss | 
-- >  p x x1 && groupBy p xs == ((x1 : xs1) : xss) .
-- >   groupBy p (x : xs) == ((x : x1 : xs1) : xss)
-- > forall p x xs x1 xs1 xss | 
-- >  not (p x x1) && groupBy p xs == ((x1 : xs1) : xss) .
-- >   groupBy p (x : xs) == [x] : (x1 : xs1) : xss
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p xs0 =
  unfoldr f xs0
  where
    f [] = Nothing
    f (x : xs) =
      let (g, xs') = span (p x) xs in
      Just (x : g, xs')

-- Adapted from teh standard library.

-- | The 'inits' function returns all initial segments of the argument,
-- shortest first.  For example:
-- 
-- > inits "abc" == ["","a","ab","abc"]
-- 
-- /O(n^2)/. Laws:
-- 
-- > inits [] == [[]]
-- > forall x xs . inits (x : xs) == map (x :) (inits xs)
inits :: [a] -> [[a]]
inits xs =
  foldr f [[]] xs
  where
    f x a = [] : map (x :) a

-- | The 'tails' function returns all final segments of the argument,
-- longest first.  For example,
-- 
-- > tails "abc" == ["abc", "bc", "c",""]
-- 
-- /O(n^2)/. Laws:
-- 
-- > tails [] == [[]]
-- > forall x xs . tails (xs ++ [x]) == map (++ [x]) (tails xs) ++ [[]]
tails :: [a] -> [[a]]
tails xs =
  foldr f [[]] xs
  where
    f x as@(a : _) = (x : a) : as

-- | The 'isPrefixOf' function takes two lists and returns
-- 'True' iff the first list is a prefix of the second.
-- This is a special case of 'isPrefixOfBy' with '(==)' as
-- the equality predicate.  /O(n)/. Laws:
-- 
-- forall ps xs . isPrefixOf ps xs == isPrefixOfBy (==) ps xs
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf xs ys = isPrefixOfBy (==) xs ys

-- | The 'isPrefixOfBy' function takes two lists and returns
-- 'True' iff the first list is a prefix of the second
-- according to the given equality predicate.
-- [New.] /O(n)/. Laws:
-- 
-- > forall p ps xs ys | stripPrefixBy p ps xs == Just ys . 
-- >   isPrefixBy p ps xs == True
-- > forall p ps xs | stripPrefixBy p ps xs == Nothing . 
-- >   isPrefixBy p ps xs == False
isPrefixOfBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isPrefixOfBy eq xs ys =
  case stripPrefixBy eq xs ys of
    Nothing -> False
    _ -> True

-- | The 'isSuffixOf' function takes two lists and returns
-- 'True' iff the first list is a suffix of the second.
-- This is a special case of 'isSuffixOfBy' with '(==)' as
-- the equality predicate.  /O(n)/. Laws:
-- 
-- forall ss xs . isSuffixOf ss xs == isSuffixOfBy (==) ss xs
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isSuffixOfBy (==) xs ys

-- | The 'isSuffixOfBy' function takes two lists and returns
-- 'True' iff the first list is a suffix of the second
-- according to the given equality predicate.
-- [New.] /O(n)/ plus the cost of
-- evaluating the predicate. Laws:
-- 
-- > forall p ss xs ys | stripSuffixBy p ss xs == Just ys . 
-- >   isSuffixOfBy p ss xs == True
-- > forall p ss xs | stripSuffixBy p ss xs == Nothing . 
-- >   isSuffixOfBy p ss xs == False
isSuffixOfBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isSuffixOfBy eq xs ys =
  case stripSuffixBy eq xs ys of
    Nothing -> False
    _ -> True

-- XXX Quadratic, but I doubt the standard library
-- is full of Boyer-Moore code.

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- iff the first list is contained, wholly and intact,
-- anywhere within the second. Some examples:
-- 
-- >isInfixOf "Haskell" "I really like Haskell." == True
-- >isInfixOf "Ial" "I really like Haskell." == False
-- 
-- This is a special case of the 'isInfixOfBy' function 
-- with '(==)' as the equality predicate. Laws:
-- 
-- > forall xs xs1 . isInfixOf xs ys == isInfixOfBy (==) xs ys
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = isInfixOfBy (==) xs ys

-- | The 'isInfixOfBy' function takes two lists and returns
-- 'True' iff the first list is contained, wholly and
-- intact, anywhere within the second, as measured by the
-- given equality predicate. Some examples:
-- 
-- >isInfixOfBy (==) "Haskell" "I really like Haskell." == True
-- >isInfixOfBy (==) "Ial" "I really like Haskell." == False
-- 
-- [New.] /O(m n)/ where /m/ is the length of the prefix and /n/ the
-- length of the list searched, plus the cost of
-- evaluating the predicate. Laws:
-- 
-- > forall p xs xs1 xs2 xs3 | xs == xs1 ++ xs2 ++ xs3 .
-- >   isInfixOfBy p xs2 xs == True
-- > forall p xs . nexists xs1 xs2 xs3 | xs == xs1 ++ xs2 ++ xs3 .
-- >   isInfixOfBy p xs2 xs == False
isInfixOfBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isInfixOfBy eq xs ys = any (isPrefixOfBy eq xs) (tails ys)

-- | 'elem' is the list membership predicate. Given an
-- element and a list, return 'True' if the element is found
-- in the list. Return 'False' otherwise. /O(n)/. Laws:
-- 
-- > forall x xs . elem x xs == elemBy (==) x xs
elem :: Eq a => a -> [a] -> Bool
elem = elemBy (==)

-- There should be an elemBy. Yes, it's just "any", but
-- still...

-- | 'elemBy' is the list membership predicate. Given an
-- element and a list, return 'True' if the element is found
-- in the list, according to the given equality
-- function. Return 'False' otherwise. [New.] /O(n)/ plus
-- the cost of evaluating the predicate. Laws:
-- 
-- > forall p x xs . elemBy p x xs == any (p x) xs
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq x0 xs =  any (eq x0) xs

-- OTOH, why is there a notElem? Did we really need that?

-- | 'elem' is the list non-membership predicate. Given an
-- element and a list, return 'False' if the element is found
-- in the list. Return 'True' otherwise. [New.] /O(n)/. Laws:
-- 
-- > forall x xs . notElem x xs == not (elem x xs)
notElem :: Eq a => a -> [a] -> Bool
notElem x0 = not . elem x0

-- | 'notElemBy' is the list non-membership predicate. Given
-- an element and a list, return 'False' if the element is
-- found in the list, according to the given equality
-- function. Return 'True' otherwise. [New.] /O(n)/ plus the
-- cost of evaluating the predicate. Laws:
-- 
-- > forall p x xs . notElemBy p x xs == not (elemBy p x xs)
notElemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy p x0 = not . elemBy p x0
  
-- | @'lookupBy' p key assocs@ looks up @key@ in
-- \"association list\" @assocs@ using the specified
-- equality predicate @p@. This is a special case of
-- 'lookupBy' with '(==)' as the equality predicate. Laws:
-- 
-- > forall k0 xs . lookup k0 xs == lookupBy (==) k0 xs
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup x0 xs =
  lookupBy (== x0) xs

-- | @'lookupBy' p key assocs@ looks up @key@ in
-- \"association list\" @assocs@ using the specified
-- equality predicate @p@. [New.] /O(n)/ plus the cost of
-- evaluating the predicate. Laws:
-- 
-- > forall p k0 . lookupBy p k0 [] == Nothing
-- > forall p k0 a xs .
-- >   lookupBy p k0 ((k0, a) : xs) == Just a
-- > forall p k0 k a xs | k /= k0 .
-- >   lookupBy p k0 ((k, a) : xs) == lookupBy p k0 xs
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p xs =
  foldr f Nothing xs
  where
    f (xk, xv) a
      | p xk = Just xv
      | otherwise = a

-- | The 'find' function takes a predicate and a list and
-- returns the 'Just' the first element in the list for
-- which the predicate hold; 'find' returns 'Nothing' if
-- there is no element for which the predicate
-- holds. /O(n)/. Laws:
-- 
-- > forall p . find p [] == Nothing
-- > forall p x xs | p x == True . find p (x : xs) == Just x
-- > forall p x xs | p x == False . find p (x : xs) == find p xs
find :: (a -> Bool) -> [a] -> Maybe a
find p xs =
  foldr f Nothing xs
  where
    f x a
      | p x = Just x
      | otherwise = a

-- | 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate (in order). /O(n)/. Laws:
-- 
-- > forall p . filter p [] == []
-- > forall p x xs | p x == True . filter p (x : xs) == x : filter p xs
-- > forall p x xs | p x == False . filter p (x : xs) == filter p xs
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = fst $ partition p xs

-- | The 'partition' function takes a predicate and a list
-- and returns the pair of lists of elements which do and do
-- not satisfy the predicate, respectively. /O(n)/. Laws:
-- 
-- > forall p xs . partition p xs == (filter p xs, filter (not . p) xs)
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs =
  foldr f ([], []) xs
  where
    f x ~(l, r)
      | p x = (x : l, r)
      | otherwise = (l, x : r)

(!!) :: Integral b => [a] -> b -> a
xs !! n = let Just x = lookup n (zip [0..] xs) in x

-- These elem and find functions mostly come from the standard library.

-- | The 'elemIndex' function returns the index of the first element
-- in the given list which is equal (by '==') to the query element,
-- or 'Nothing' if there is no such element. This is a special
-- case of 'elemIndexBy'. Laws:
-- 
-- forall x xs . elemIndex x xs == elemIndexBy (==) x xs
elemIndex       :: Eq a => a -> [a] -> Maybe Int
elemIndex x xs = elemIndexBy (==) x xs

-- | The 'elemIndexBy' function returns the index of the
-- first element in the given list which is equal (by the
-- given predicate) to the query element, or 'Nothing' if
-- there is no such element. This is a special case of
-- 'findIndex'. [New.] /O(n)/ plus the cost of evaluating the
-- predicate. Laws:
-- 
-- forall p x xs . elemIndexBy p x xs == findIndex (p x) xs
elemIndexBy :: (a -> a -> Bool) -> a -> [a] -> Maybe Int
elemIndexBy eq x xs = findIndex (eq x) xs

-- | The 'elemIndices' function extends 'elemIndex', by
-- returning the indices of all elements equal (by ==) to
-- the query element, in ascending order. /O(n)/. Laws:
-- 
-- > forall x xs . elemIndices x xs == elemIndicesBy (==) x xs
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x xs = elemIndicesBy (==) x xs

-- | The 'elemIndicesBy' function extends 'findIndices', by
-- returning the indices of all elements equal (by the given
-- equality function) to the query element, in ascending
-- order. [New.] /O(n)/ plus the cost of evaluating the
-- predicate. Laws:
-- 
-- > forall p x xs . elemIndicesBy p x xs == findIndicesBy (p x) xs
elemIndicesBy :: (a -> a -> Bool) -> a -> [a] -> [Int]
elemIndicesBy p x xs = findIndices (p x) xs

-- | The 'findIndex' function takes a predicate and a list and returns
-- the index of the first element in the list satisfying the predicate,
-- or 'Nothing' if there is no such element. It is a special
-- case of 'findIndex' which returns just the first index. /O(n)/. Laws:
-- 
-- > forall p xs | findIndices p xs == [] . 
-- >   findIndex p xs == Nothing
-- > forall p xs y ys | findIndices p xs == (y : ys) . 
-- >   findIndex p xs == Just y
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = 
  case findIndices p xs of
    [] -> Nothing
    (x : _) -> Just x

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs =
  map fst $ filter (p . snd) $ zip [0..] xs

-- This idea comes from the standard library.

zip :: [a] -> [b] -> [(a, b)]
zip =  zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 =  zipWith3 (,,)

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 =  zipWith4 (,,,)

-- No, I don't believe anyone uses higher-arity zips, so there.

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs1 xs2 =
  unfoldr g (xs1, xs2)
  where
    g (l1 : l1s, l2 : l2s) = Just (f l1 l2, (l1s, l2s))
    g _ = Nothing

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f xs1 xs2 = zipWith ($) (zipWith f xs1 xs2)

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f xs1 xs2 xs3 = zipWith ($) (zipWith3 f xs1 xs2 xs3)

unzip :: [(a, b)] -> ([a], [b])
unzip =
  foldr f ([], [])
  where
    f (a, b) (as, bs) = (a : as, b : bs)

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr f ([], [], [])
  where
    f (a, b, c) (as, bs, cs) = (a : as, b : bs, c : cs)

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
  foldr f ([], [], [], [])
  where
    f (a, b, c, d) (as, bs, cs, ds) = (a : as, b : bs, c : cs, d : ds)

lines :: String -> [String]
lines "" = []
lines s =
  unfoldr f (Just s)
  where
    f Nothing = Nothing
    f (Just cs) =
      let (l, r) = break (== '\n') cs in
      case r of
        ('\n' : ls@(_ : _)) -> Just (l, Just ls)
        _ -> Just (l, Nothing)

words :: String -> [String]
words s0 =
  unfoldr f s0
  where
    f s
      | null ws = Nothing
      | otherwise =
        let (w, ws') = break isSpace ws in
        Just (w, ws')
      where
        (_, ws) = span isSpace s

unlines :: [String] -> String
unlines [] = ""
unlines ls = intercalate "\n" ls ++ "\n"

unwords :: [String] -> String
unwords ws = intercalate " " ws

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

-- Instead of (\\)
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff = listDiffBy (==)

listDiff' :: Eq a => [a] -> [a] -> [a]
listDiff' = listDiffBy' (==)

union :: Eq a => [a] -> [a] -> [a]
union = unionBy (==)

union' :: Eq a => [a] -> [a] -> [a]
union' = unionBy' (==)

intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' = intersectBy' (==)

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

sort :: Ord a => [a] -> [a]
sort = sortBy compare

insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

insert' :: Ord a => a -> [a] -> [a]
insert' = insertBy' compare

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy f =
  snd . fold g ([], [])
  where
    g x (l, rs)
      | elemBy f x l = (l, rs)
      | otherwise = (x : l, x : rs)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy p t es =
  snd $ fold f (True, []) es
  where
    f x (l, r)
      | l && p x t = (False, r)
      | otherwise = (l, x : r)

listDiffBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listDiffBy f xs ys =
  foldl (flip (deleteBy f)) xs ys

-- This definition of listDiffBy makes the result canonical
-- on all inputs.
listDiffBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listDiffBy' f xs ys =
  filter (\x -> notElemBy f x (nubBy f ys)) (nubBy f xs)

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy f xs ys =
  xs ++ listDiffBy f (nubBy f ys) xs

-- The standard definition of unionBy is maximally lazy:
-- this one makes the result canonical on all inputs.
unionBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy' f xs ys =
  let xs' = nubBy f xs in
  xs' ++ listDiffBy f (nubBy f ys) xs'

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy f xs ys =
  filter (\x -> elemBy f x ys) xs

-- This definition of intersectBy makes the result canonical
-- on all inputs.
intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' f xs ys =
  filter (\x -> elemBy f x (nubBy f ys)) (nubBy f xs)


-- This should be in the standard library anyhow.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy c xs1 xs2 =
  unfoldr f (xs1, xs2)
  where
    f ([], []) = Nothing
    f ([], x2 : x2s) = Just (x2, ([], x2s))
    f (x1 : x1s, []) = Just (x1, (x1s, []))
    f (x1 : x1s, x2 : x2s)
      | x1 `c` x2 == GT = Just (x2, (x1 : x1s, x2s))
      | otherwise = Just (x1, (x1s, x2 : x2s))

-- This seems to need the general power of unfold
-- to get its work done? It's a O(n log n) merge
-- sort, although probably not as fast as the one
-- in the standard library.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy c xs0 =
  case fst $ unfold f (map (: []) xs0, undefined) of
    [] -> []
    [xs] -> xs
  where
    f ([], _) = ([], Nothing)
    f ([xs], _) = ([xs], Nothing)
    f (xss, _) =
      (sortStep, Just undefined)
      where
        -- Assume a list of sorted inputs. Merge adjacent
        -- pairs of lists in the input to produce about half
        -- as many sorted lists, each about twice as large.
        sortStep =
          unfoldr g xss
          where
            g [] = Nothing
            g [xs] =
              Just (xs, [])
            g (xs1 : xs2 : xs) =
              Just (mergeBy c xs1 xs2, xs)

-- This version of insertBy actually follows the contract
-- from the documentation (inasmuch as that contract can be
-- read to specify anything sensible.) This version is
-- maximally productive. It is non-recursive. It is ugly and
-- kludgy.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy c t xs0 =
  let (xs1, xs2) = span (\x -> t `c` x /= LT) xs0 in
  let xs2' =
        case foldr f (Left []) xs2 of
          Left xs -> t : xs
          Right xs -> xs in
  xs1 ++ xs2'
  where
    f x (Left []) = Left [x]
    f x (Left [x']) | t `c` x' /= LT = Right [x, x', t]
    f x (Left xs) | t `c` x /= LT = Right (x : t : xs)
    f x (Left xs) = Left (x : xs)
    f x (Right xs) = Right (x : xs)

-- This version of insertBy agrees with the standard
-- library, which inserts in the first possible location
-- rather than the last. (This is bug #7421 in the GHC
-- Trac.)
insertBy' :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy' c t xs =
  let (l, r) = span ((== GT) . c t) xs in
  l ++ [t] ++ r

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy c xs =
  foldl1' f xs
  where
    x1 `f` x2
      | x1 `c` x2 == LT = x2
      | otherwise = x1

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy c xs =
  foldl1' f xs
  where
    x1 `f` x2
      | x1 `c` x2 == GT = x2
      | otherwise = x1

-- The rest of the functions are already generic,
-- because why not? Length not so much, since one
-- tends to use it to anchor type constraints.
genericLength :: Num a => [b] -> a
genericLength xs = foldl' (\a _ -> a + 1) 0 xs
