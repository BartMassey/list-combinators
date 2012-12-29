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
-- implementation of GHC. However, this is not a
-- lightly-hacked 'Data.List':
-- 
--   * The documentation has been substantially rewritten to
--   provide a more accurate description
--   
--   * The primitives have been almost entirely
--   reimplemented, with the goal of eliminating as many
--   recursive constructions as possible.  Most primitives
--   are now implemented in terms of each other; the
--   remaining few are implemented in terms of a generalized
--   linear recursion 'lr' operation.
-- 
--   * The handling of primitives involving numbers has
--   changed to use 'Integer' rather than 'Int' as the
--   default type. This future-proofs the library and
--   makes the Laws of these operations easier to use,
--   presumably at the expense of performance.
--   
--   * Quite a few new primitives have been added, to
--     fill holes in the library or just for symmetry
--     with existing primitives.
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
-- 
-- Each function has "laws" that are intended to specify its
-- behavior.  The laws are currently only intended to cover
-- the case of finite lists; they may or may not work in the
-- presence of infinite lists even if the function being
-- specified is maximally productive.
-----------------------------------------------------------------------------

module Data.List.Combinator (
  module Prelude,
  -- * Linear Recursion
  lr,
  -- * Basic Functions
  (++),
  head,
  last,
  tail,
  init,
  null,
  length,
  length',
  genericLength,
  -- * List Transformations
  map,
  reverse,
  intersperse,
  separate,
  separate',
  terminate,
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
  unfoldr,
  unfoldl,
  unfoldl',
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
  -- * Indexing Lists
  (!!),
  elemIndex,
  elemIndices,
  findIndex,
  findIndices,
  -- * Zipping and Unzipping Lists
  zip,
  zip3,
  zip4,
  zip5,
  zip6,
  zip7,
  zipWith,
  zipWith3,
  zipWith4,
  zipWith5,
  zipWith6,
  zipWith7,
  unzip,
  unzip3,
  unzip4,
  unzip5,
  unzip6,
  unzip7,
  -- * Special Lists
  lines,
  words,
  unlines,
  unwords,
  -- * \"Set\" Operations
  nub,
  delete,
  (\\),
  (\\*),
  union,
  union',
  intersect,
  intersect',
  -- * Ordered Lists
  merge,
  sort,
  insert,
  insert',
  -- * The \"By\" Operations
  separateBy,
  separateBy',
  terminateBy,
  elemBy,
  notElemBy,
  nubBy,
  deleteBy,
  deleteFirstsBy,
  deleteFirstsBy',
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
  elemIndicesBy) where

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
  unwords)
import Data.Char (isSpace)


-- XXX Need to write laws for this.

-- | Generalized linear recursion.  Given an initial left
-- and right accumulator and a function that steps the
-- accumulators forward from the left and right as with
-- 'fold', keep stepping until the function indicates
-- completion by returning 'Nothing' on the right. /O(n)/
-- where /n/ is the number of recursive steps, plus the cost
-- of evaluating the folding function.
-- 
-- This abstraction of 'fold' is a bidirectional
-- generalization of 'unfoldr': both are written as
-- 'lr's.
lr :: ((l, r) -> (l, Maybe r)) -> (l, r) -> (l, r)
lr f (l, r) =
  let (l1, mr1) = f (l, r2)
      (l2, r2) = lr f (l1, r) in
  case mr1 of
    Nothing -> (l1, r)
    Just r1 -> (l2, r1)


-- XXX Need to write laws for this.

-- | Given a function that accepts an element and a left and
-- right context and produces a new left and right context,
-- and given an initial left and right context and a list,
-- run the function on each element of the list with the
-- appropriate context.
-- 
-- The 'fold' operation generalizes a number of things from
-- 'Data.List', including 'foldl' and 'foldr'. It works by
-- allowing `f` to work with both state accumulated from the
-- left and state built up from the right simultaneously.
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
-- There is probably a need for a version of this function
-- element-strict in the left context: call it 'fold'' .
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
fold f (l0, r0) xs0 =
  let ((l, _), r) = lr g ((l0, xs0), r0) in
  (l, r)
  where
    g ((l, []), _) = ((l, undefined), Nothing)
    g ((l, x : xs), r) =
      let (l', r') = f x (l, r) in
      ((l', xs), Just r')

-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
--
-- /O(m)/. Laws:
-- 
-- > forall xs . [] ++ xs == xs
-- > forall x xs ys . x : (xs ++ ys) == (x : xs) ++ ys
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

-- | Return the first element of a non-empty
-- list. /O(1)/. Laws:
-- 
-- > forall l . not (null l) . (head l : tail l) == l
head :: [a] -> a
head (x : _) = x
head _ = error "head: empty list"

-- | Return the last element of a non-empty
-- list. Strict. /O(1)/. Laws:
-- 
-- > forall x xs . last (xs ++ [x]) == x
last :: [a] -> a
last (x : xs) = foldl (\_ y -> y) x xs
last _ = error "last: empty list"

-- | Return the second and subsequent elements of a
-- non-empty list. /O(1)/. Laws:
-- 
-- > forall x xs . tail (x : xs) == xs
tail :: [a] -> [a]
tail (_ : xs) = xs
tail _ = error "tail: empty list"

-- | Return all the elements of a non-empty list except the
-- last one. /O(1)/. Laws:
-- 
-- > forall x xs . init (xs ++ [x]) == xs
init :: [a] -> [a]
init xs0 =
  foldr f [] xs0
  where
    f _ [] = []
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
-- > forall x xs . length (x : xs) == 1 + length xs
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

-- | Returns the length of a list with less than or equal to
-- @'maxBound' t@ elements as a value of type @t@, where @t@
-- is an 'Integral' type being used for list length. Does
-- something undefined on longer lists. Not the default type
-- for the length function, since type inference tends to
-- get lost and default badly if the length function does
-- not constrain it. Strict. /O(n)/. Laws:
-- 
-- > length' [] == 0
-- > forall x xs | length xs < maxBound :: Int .
-- >   length' (x : xs) == 1 + length' xs
genericLength :: Integral a => [b] -> a
genericLength xs = foldl' (\a _ -> a + 1) 0 xs

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
intersperse s (x : xs) = 
  x : foldr (\x' a -> s : x' : a) [] xs

-- | The 'separate' function takes an element and a list;
-- it treats the element as a \"field separator\" and
-- splits the list into fields (sublists) at every occurrence.
-- If the input list is empty, no fields are returned.
-- Some examples:
-- 
-- > separate ',' ",a,b,c,d," == ["","a","b","c","d",""]
-- > separate ',' "" == []
-- 
-- 'separate' is a special case of 'separateBy' with
-- predicate @(== t)@. It is also a special case of
-- 'separate'' that handles the empty list slightly
-- differently. New. /O(n)/. Laws:
-- 
-- > forall t xs . separate t xs == separateBy (== t) xs
separate :: Eq a => a -> [a] -> [[a]]
separate t xs = separateBy (== t) xs

-- | The 'separateBy' function takes a predicate @p@ and a list;
-- it treats any element of the list for which @p@
-- returns true as a \"field separator\" and splits the list
-- into fields (sublists) at each such occurrence.  If the
-- input list is empty, no fields are returned.  Some
-- examples:
-- 
-- > separateBy (not . isAlpha) "&a+b$" == ["","a","b",""]
-- > separateBy (not . isAlpha) "" == []
-- 
-- 'separateBy' is a special case of 'separateBy'' that
-- handles the empty list slightly
-- differently. New. /O(n)/. Laws:
-- 
-- > forall p . separateBy p [] = []
-- > forall p x xs . separateBy p (x : xs) = separateBy' p (x : xs)
separateBy :: (a -> Bool) -> [a] -> [[a]]
separateBy _ [] = []
separateBy p xs = separateBy' p xs

-- | The 'separate'' function takes an element and a list;
-- it treats the element as a \"field separator\" and
-- splits the list into fields (sublists) at every occurrence.
-- If the input list is empty, a single empty field is returned.
-- Some examples:
-- 
-- > separate' ',' ",a,b,c,d," == ["","a","b","c","d",""]
-- > separate' ',' "" == [""]
-- 
-- 'separate'' is a special case of 'separateBy'' with
-- predicate @(== t)@. New. /O(n)/. Laws:
-- 
-- > forall t . separate t [] == []
separate' :: Eq a => a -> [a] -> [[a]]
separate' t xs = separateBy' (== t) xs

-- | The 'separateBy'' function takes a predicate @p@ and a list;
-- it treats any element of the list for which @p@
-- returns true as a \"field separator\" and splits the list
-- into fields (sublists) at each such occurrence.  If the
-- input list is empty, a single empty field is returned.  Some
-- examples:
-- 
-- > separateBy (not . isAlpha) "&a+b$" == ["","a","b",""]
-- > separateBy (not . isAlpha) "" == [""]
-- 
-- New. /O(n)/. Laws:
-- 
-- > forall p . separateBy' p [] = [[]]
-- > forall p x xs | p x . 
-- >   separateBy' p (x : xs) == [] : separateBy' p xs
-- > forall p x xs ys yss | 
-- >  not (p x) && (ys : yss) == separateBy' p xs .
-- >   separateBy' p (x : xs) = (x : ys) : yss
separateBy' :: (a -> Bool) -> [a] -> [[a]]
separateBy' p xs =
  foldr f [[]] xs
  where
    f x a | p x = [] : a
    f x ~(ys : yss) = (x : ys) : yss

-- | The 'terminate' function takes an element and a list;
-- it treats the element as a \"field terminator\" and
-- splits the list into fields (sublists) before every occurrence.
-- If the input list is empty, no fields are returned.
-- If the input list is nonempty but does not end with a
-- terminator, an implicit final terminator is assumed.
-- Some examples:
-- 
-- > terminate ';' "a;b;c;d;" == ["a","b","c","d"]
-- > terminate ';' "a;b" == ["a","b"]
-- > terminate ';' ";" == [""]
-- > terminate ';' [] == []
-- 
-- 'terminate' is a special case of 'terminateBy' with
-- predicate @(== t)@. New. /O(n)/. Laws:
-- 
-- > forall t xs . terminate t xs == terminateBy (== t) xs
terminate :: Eq a => a -> [a] -> [[a]]
terminate t xs = terminateBy (== t) xs

-- | The 'terminateBy' function takes a predicate @p@ and a list;
-- it treats any  element for which @p@ holds as a \"field terminator\" and
-- splits the list into fields (sublists) before each such occurrence.
-- If the input list is empty, no fields are returned.
-- If the input list is nonempty but does not end with a
-- terminator, an implicit final terminator is assumed.
-- Some examples:
-- 
-- > terminateBy (`elem` ";.") "a;b." == ["a","b"]
-- > terminateBy (`elem` ";.") "a;b" == ["a","b"]
-- > terminateBy (`elem` ";.") "" == []
-- 
-- New. /O(n)/. Laws:
-- 
-- > forall p . terminateBy p [] = []
-- > forall p x | p x . terminateBy p [x] == [[]]
-- > forall p x | not (p x) . terminateBy p [x] == [[x]]
-- > forall p x1 x2 xs yss | 
-- >  p x1 && yss == terminateBy p (x2 : xs) .
-- >   terminateBy p (x1 : x2 : xs) == [] : yss
-- > forall p x1 x2 xs ys yss | 
-- >  not (p x1) && (ys : yss) == terminateBy p (x2 : xs) .
-- >   terminateBy p (x1 : x2 : xs) == (x1 : ys) : yss
terminateBy :: (a -> Bool) -> [a] -> [[a]]
terminateBy p xs =
  case foldr f Nothing xs of
    Nothing -> []
    Just ys -> ys
  where
    f x Nothing | p x = Just [[]]
    f x Nothing = Just [[x]]
    f x (Just a) | p x = Just ([] : a)
    f x (Just ~(ys : yss)) = Just ((x : ys) : yss)

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
        g ~(y : ys) (h, t) = (y : h, ys : t)

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
-- right. New. /O(n^2)/. Laws:
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

-- XXX The strictness isn't right here currently, maybe?

-- | The semantics of 'foldl'' those of 'foldl', except that
-- 'foldl'' eagerly applies @f@ at each step. This gains
-- some operational efficiency, but makes 'foldl'' only
-- partially correct: there are cases where 'foldl' would
-- return a value, but 'foldl'' on the same arguments will
-- nonterminate or fail. The Laws for 'foldl'' are thus
-- the same as those for 'foldl' up to a condition on @f@,
-- which is not given here.
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a0 =
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
foldl1 _ _ = error "foldl1: empty list"

-- | 'foldl1'' bears the same relation to 'foldl1' that
-- 'foldl'' bears to 'foldl'.
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x : xs) = foldl' f x xs 
foldl1' _ _ = error "foldl1': empty list"

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
-- non-empty list of 'Ord' elements. If there are multiple
-- maxima, the choice of which to return is
-- unspecified. This is a special case of 'maximumBy' with
-- comparison function 'compare'. Strict. /O(n)/. Laws:
-- 
-- > forall xs | not (null xs) . maximum xs == maximumBy compare xs
maximum :: Ord a => [a] -> a
maximum xs = maximumBy compare xs

-- | The 'maximum' function returns a maximum value from a
-- non-empty list of elements using a provided comparison
-- function. If there are multiple maxima, the choice of
-- which to return is unspecified. Strict. /O(n)/. Laws:
-- 
-- > forall c x . maximumBy c [x] == x
-- > forall c x xs | 
-- >  not (null xs) && c x (maximumBy c xs) /= LT .
-- >   maximumBy c (x : xs) == x
-- > forall c x xs | 
-- >  not (null xs) && c x (maximumBy c xs) == LT .
-- >   maximumBy c (x : xs) == maximumBy c xs
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy c xs =
  foldl1' f xs
  where
    x1 `f` x2
      | x1 `c` x2 == LT = x2
      | otherwise = x1

-- | The 'minimum' function returns a minimum value from a
-- non-empty list of 'Ord' elements.  If there are multiple
-- minima, the choice of which to return is
-- unspecified. This is a special case of 'minimumBy' with
-- comparison function 'compare'. Strict. /O(n)/. Laws:
-- 
-- > forall xs | not (null xs). minimum xs == minimumBy compare xs
minimum :: Ord a => [a] -> a
minimum xs = minimumBy compare xs

-- | The 'minimum' function returns a minimum value from a
-- non-empty list of 'Ord' elements
-- function. If there are multiple minima, the choice of
-- which to return is unspecified. Strict. /O(n)/. Laws:
-- 
-- > forall c x . minimumBy c [x] == x
-- > forall c x xs | 
-- >  not (null xs) && c x (minimumBy c xs) /= GT .
-- >   minimumBy c (x : xs) == x
-- > forall c x xs | 
-- >  not (null xs) && c x (minimumBy c xs) == GT .
-- >   minimumBy c (x : xs) == minimumBy c xs
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy c xs =
  foldl1' f xs
  where
    x1 `f` x2
      | x1 `c` x2 == GT = x2
      | otherwise = x1

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
scanl1 _ _ = error "scanl1: empty list"

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
    f' _ _ = error "scanr: internal error: fell off end"

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
      let (l', r') = f l x in
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
  snd $ lr g (a0, [])
  where
    g (a, xs) =
      case f a of
        Nothing -> 
          (a, Nothing)
        Just (x, a') ->
          (a', Just (x : xs))

-- | An 'unfoldl' is provided for completeness. New.
-- Spine-strict. /O(n)/ plus the cost of evaluating the
-- unfolding function. Laws:
-- 
-- > forall f a . unfoldl f a == reverse (unfoldr (fmap flip f) a)
unfoldl :: (a -> Maybe (a, b)) -> a -> [b]
unfoldl f a0 =
  snd $ fst $ lr g ((a0, []), undefined)
  where
    g ((a, xs), _) =
      case f a of
        Nothing -> 
          ((a, xs), Nothing)
        Just (a', x) ->
          ((a', x : xs), Just undefined)

-- | An 'unfoldl'' is provided for the same reasons as
-- 'foldl''.  Element-strict and spine-strict. /O(n)/
-- plus the cost of evaluating the unfolding function.
unfoldl' :: (a -> Maybe (a, b)) -> a -> [b]
unfoldl' f a0 =
  snd $ fst $ lr g ((a0, []), undefined)
  where
    g ((a, xs), _) =
      case f a of
        Nothing -> 
          ((a `seq` a, xs), Nothing)
        Just (a', x) ->
          ((a' `seq` a', x : xs), Just undefined)


-- XXX Strictness bug:
-- 
-- >>> take 1 $ insertBy' compare 2 [1, undefined]
-- [1*** Exception: Prelude.undefined
-- 
-- After messing with it for an hour, I have no clue why.

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
take n xs = fst $ splitAt n xs

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
drop n xs = snd $ splitAt n xs

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
splitAt n0 xs | n0 < 0 = 
  ([], xs)
splitAt n0 xs =
  snd $ fold f (n0, ([], [])) xs
  where
    f x (0, ~(_, rs)) = (0, ([], x : rs))
    f x (n, ~(ls, rs)) = (n - 1, (x : ls, rs))

-- | Returns a list of all possible splits of its list
-- argument as produced by 'splitAt' in order of
-- increasing @n@. New. /O(n)/. Laws:
-- 
-- > forall n xs | length xs == n .
-- >   splits xs == zipWith splitAt [0..n] (repeat xs)
splits :: [a] -> [([a], [a])]
splits xs0 =
  snd $ fold f (0 :: Integer, [(xs0, [])]) xs0
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
-- New. /O(n)/ plus the cost of evaluating the predicate. Laws:
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
-- New. /O(n)/ plus the cost of evaluating the predicate. Laws:
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
-- New. /O(n)/ plus the cost of evaluating the predicate. Laws:
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
-- New. /O(m n)/ where /m/ is the length of the prefix and /n/ is the
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
        f (ok, (y : ys')) x = (ok && eq x y, ys')

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
    f _ _ = error "tails: internal error: fell off end"

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
-- New. /O(n)/. Laws:
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
-- New. /O(n)/ plus the cost of
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
-- New. /O(m n)/ where /m/ is the length of the prefix and /n/ the
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
-- function. Return 'False' otherwise. New. /O(n)/ plus
-- the cost of evaluating the predicate. Laws:
-- 
-- > forall p x xs . elemBy p x xs == any (p x) xs
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq x0 xs =  any (eq x0) xs

-- OTOH, why is there a notElem? Did we really need that?

-- | 'elem' is the list non-membership predicate. Given an
-- element and a list, return 'False' if the element is found
-- in the list. Return 'True' otherwise. New. /O(n)/. Laws:
-- 
-- > forall x xs . notElem x xs == not (elem x xs)
notElem :: Eq a => a -> [a] -> Bool
notElem x0 = not . elem x0

-- | 'notElemBy' is the list non-membership predicate. Given
-- an element and a list, return 'False' if the element is
-- found in the list, according to the given equality
-- function. Return 'True' otherwise. New. /O(n)/ plus the
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
-- equality predicate @p@. New. /O(n)/ plus the cost of
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

-- | List index (subscript) operator, starting from 0. /O(n)/ where
-- /n/ is the right-hand argument. Laws:
-- 
-- > forall x xs . (x : xs) !! 0 == x
-- > forall n x xs | 
-- >  n > 0 && n < length (x : xs) .
-- >   (x : xs) !! n == xs !! (n - 1)
(!!) :: Integral b => [a] -> b -> a
_ !! n | n < 0 =
  error "!!: negative index"
xs !! n = 
  case lookup n (zip [0..] xs) of
    Just x -> x
    Nothing -> error "!!: index too large"

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
-- 'findIndex'. New. /O(n)/ plus the cost of evaluating the
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
-- order. New. /O(n)/ plus the cost of evaluating the
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

-- | The 'zip'function takes two lists and returns a list of
-- corresponding pairs. If one input list is short, excess
-- elements of the longer list are discarded. 'zip' is a 
-- special case of 'zipWith' with the tupling function @(,)@ 
-- as the operator. /O(n)/. Laws:
-- 
-- forall xs . zip [] xs == []
-- forall xs . zip xs [] == []
-- forall x xs y ys . zip (x : xs) (y : ys) == (x, y) : zip xs ys
zip :: [a] -> [b] -> [(a, b)]
zip =  zipWith (,)

-- | 'zip' from 3 lists to 3-tuples.
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 =  zipWith3 (,,)

-- | 'zip' from 4 lists to 4-tuples.
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 =  zipWith4 (,,,)

-- | 'zip' from 5 lists to 5-tuples.
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 =  zipWith5 (,,,,)

-- | 'zip' from 6 lists to 6-tuples.
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 =  zipWith6 (,,,,,)

-- | 'zip' from 7 lists to 7-tuples.
zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] 
     -> [(a, b, c, d, e, f, g)]
zip7 =  zipWith7 (,,,,,,)

-- I don't believe anyone uses higher-arity zips, but I feel constrained
-- by the standard library.


-- | The 'zipWith' function generalises 'zip' by zipping
-- with the function given as the first argument, instead of
-- a tupling function. For example, @'zipWith' (+)@ is
-- applied to two lists to produce the list of corresponding
-- sums. /O(n)/. Laws: 
-- 
-- forall f xs . zipWith f [] xs == []
-- forall f xs . zipWith f xs [] == []
-- forall f x xs y ys . zipWith (x : xs) (y : ys) == f x y : zipWith xs ys
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs1 xs2 =
  unfoldr g (xs1, xs2)
  where
    g (l1 : l1s, l2 : l2s) = Just (f l1 l2, (l1s, l2s))
    g _ = Nothing

-- | 'zipWith' with 3 lists and a 3-argument function.
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f xs1 xs2 = zipWith ($) (zipWith f xs1 xs2)

-- | 'zipWith' with 4 lists and a 4-argument function.
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f xs1 xs2 xs3 = zipWith ($) (zipWith3 f xs1 xs2 xs3)

-- | 'zipWith' with 5 lists and a 5-argument function.
zipWith5 :: (a -> b -> c -> d -> e -> f) 
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 f xs1 xs2 xs3 xs4 = 
  zipWith ($) (zipWith4 f xs1 xs2 xs3 xs4)

-- | 'zipWith' with 6 lists and a 6-argument function.
zipWith6 :: (a -> b -> c -> d -> e -> f -> g) 
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 f xs1 xs2 xs3 xs4 xs5 = 
  zipWith ($) (zipWith5 f xs1 xs2 xs3 xs4 xs5)

-- | 'zipWith' with 7 lists and a 7-argument function.
zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) 
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 f xs1 xs2 xs3 xs4 xs5 xs6 = 
  zipWith ($) (zipWith6 f xs1 xs2 xs3 xs4 xs5 xs6)

-- | The 'unzip' function transforms a list of pairs into a
-- list of first components and a list of second
-- components. /O(n)/. Laws:
-- 
-- > unzip [] == ([], [])
-- > forall a b xs as bs | (as, bs) == unzip xs . 
-- >   unzip ((a, b) : xs) == (a : as, b : bs)
unzip :: [(a, b)] -> ([a], [b])
unzip =
  foldr f ([], [])
  where
    f (a, b) (as, bs) = (a : as, b : bs)

-- | Unzip a 3-tuple into 3 lists.
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr f ([], [], [])
  where
    f (a, b, c) (as, bs, cs) = (a : as, b : bs, c : cs)

-- | Unzip a 4-tuple into 4 lists.
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
  foldr f ([], [], [], [])
  where
    f (a, b, c, d) (as, bs, cs, ds) = (a : as, b : bs, c : cs, d : ds)

-- | Unzip a 5-tuple into 5 lists.
unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 =
  foldr f ([], [], [], [], [])
  where
    f (a, b, c, d, e) (as, bs, cs, ds, es) = 
      (a : as, b : bs, c : cs, d : ds, e : es)

-- | Unzip a 6-tuple into 6 lists.
unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 =
  foldr f0 ([], [], [], [], [], [])
  where
    f0 (a, b, c, d, e, f) (as, bs, cs, ds, es, fs) = 
      (a : as, b : bs, c : cs, d : ds, e : es, f : fs)

-- | Unzip a 7-tuple into 7 lists.
unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 =
  foldr f0 ([], [], [], [], [], [], [])
  where    
    f0 (a, b, c, d, e, f, g) (as, bs, cs, ds, es, fs, gs) = 
      (a : as, b : bs, c : cs, d : ds, e : es, f : fs, g : gs)

-- | The 'lines' function breaks a string up into a list of
-- strings at newline characters. A trailing newline
-- character will not cause an empty string at the end of
-- the list, unless the input string consists entirely of
-- newlines. The resulting strings do not contain newlines.
-- /O(n)/. 'lines' is a special case of 'terminate' with 
-- terminator '\n'. Laws:
-- 
-- > forall xs . lines xs == terminate '\n' xs
lines :: String -> [String]
lines xs = terminate '\n' xs

-- | The 'words' function breaks a string up into a list of
-- words at sequences of white space (as defined by
-- 'Data.Char.isSpace'), discarding any leading or trailing
-- whitespace. /O(n)/. Laws:
-- 
-- > forall xs | all isSpace xs . words xs == []
-- > forall xs ys z zs 
-- >  | all isSpace xs &&
-- >    all (not . isSpace) ys &&
-- >    isSpace z .
-- >   words (xs ++ ys ++ (z : zs)) == ys : words (z : zs)
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

-- | The 'unlines' function appends a newline to each
-- element of its string list argument and concatenates
-- the result. /O(n)/. Laws:
-- 
-- > unlines [] == ""
-- > forall xs xss . 
-- >   unlines (xs : xss) == concatMap (++ "\n") (xs : xss)
unlines :: [String] -> String
unlines [] = ""
unlines xss = concatMap (++ "\n") xss

-- | The 'unwords' function places a space character between each
-- element of its string list argument and concatenates
-- the result. /O(n)/. Laws:
-- 
-- > forall xss . unwords xss == intercalate " " xss
unwords :: [String] -> String
unwords xss = intercalate " " xss

-- | The 'nub' function removes duplicate elements from a
-- list.  In particular, it keeps only the first occurrence
-- of each element.  (The name 'nub' means \"essence\".)  It
-- is a special case of 'nubBy' with @(==)@ as the equality
-- test. /O(n^2)/. Laws:
-- 
-- > forall xs . nub xs == nubBy (==) xs
nub :: Eq a => [a] -> [a]
nub = nubBy (==)

-- | The 'nub' function removes duplicate elements from a
-- list.  In particular, it keeps only the first occurrence
-- of each element, as judged by the given equality predicate.  
-- (The name 'nub' means \"essence\".)
-- /O(n^2)/ plus the cost of evaluating the predicate. Laws:
-- 
-- > forall p . nubBy p [] == []
-- > forall p x xs . nubBy p (x : xs) == x : filter (not . p) (nubBy p xs)
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy p xs =
  snd $ fold g ([], []) xs
  where
    g x (l, rs)
      | elemBy p x l = (l, rs)
      | otherwise = (x : l, x : rs)

-- | The 'delete' function deletes the first occurrence, if
-- any, of a given element from a given list. It is a
-- special case of 'deleteBy' with @(==)@ as the equality
-- test. /O(n)/. Laws:
-- 
-- > forall x xs . delete x xs == deleteBy (==) x xs
delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

-- | The 'deleteBy' function deletes the first occurrence, if
-- any, of a given element from a given list, using
-- the supplied equality test. Some examples:
-- 
-- > delete 'a' "banana" == "bnana"
-- > delete 'a' "frog" == "frog"
-- 
-- /O(n)/. Laws:
-- 
-- > forall p x . deleteBy p x [] == []
-- > forall p x xs | p x == True . 
-- >   deleteBy p x xs == xs
-- > forall p x xs | p x == False . 
-- >   deleteBy p x xs == x : deleteBy p xs
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy p x0 xs0 =
  let (xs, ys) = break (p x0) xs0 in
  xs ++ safeTail ys
  where
    safeTail [] = []
    safeTail (_ : ys) = ys

-- | The '\\' operator is non-associative list difference.
-- The result of @xs '\\' ys@ is @xs@ with the first
-- occurence (if any) of each element of @ys@ removed. Thus
-- 
-- > (xs ++ ys) \\ xs == ys
-- 
-- The '\\' operator is a special case of the
-- 'deleteFirstsBy' function with equality operator
-- @(==)@. /O(m n)/. Laws:
-- 
-- > forall xs ys . xs \\ ys == deleteFirstsBy (==) xs ys
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = deleteFirstsBy (==) xs ys

-- | The 'deleteFirstsBy' function is non-associative list
-- difference under a user-provided equality predicate.  The
-- result of @'deleteFirstsBy' xs ys@ is @xs@ with the first
-- occurence (if any, as measured by the supplied equality
-- predicate) of each element of @ys@ removed. Thus, for any
-- well-formed equality predicate @p@
-- 
-- > deleteFirstBy p (xs ++ ys) xs == ys
-- 
-- /O(m n)/ plus the cost of evaluating the equality predicate. Laws:
-- 
-- > forall p xs . deleteFirstsBy p xs [] == xs
-- > forall p xs (y : ys) . deleteFirstsBy p (deleteBy p y xs) ys
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy p xs ys =
  foldl (flip (deleteBy p)) xs ys

-- | The '\\*' operator is like the `\\` operator, except
-- that both arguments are made canonical by elimination of
-- duplicate elements before differencing. /O(m n)/. Laws:
-- 
-- > forall xs ys . xs \\* ys == deleteFirstsBy' (==) xs ys
(\\*) :: Eq a => [a] -> [a] -> [a]
xs \\* ys = deleteFirstsBy' (==) xs ys

-- | This variant of 'deleteFirstsBy' makes the result
-- canonical on all inputs. /O(m n) + O(m^2) + O(n^2)/ plus
-- the cost of evaluating the equality predicate. Laws:
-- 
-- > forall p xs ys . 
-- >   deleteFirstsBy' p xs ys == deleteFirstsBy p (nub xs) (nub ys)
deleteFirstsBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy' p xs ys =
  deleteFirstsBy p (nubBy p ys) (nubBy p xs)

-- | The 'union' function returns the \"list union\" of
-- two lists. It is a special case of 'unionBy' with @(==)@
-- as the equality predicate. /O(m n)/. Laws:
-- 
-- > forall xs ys . union xs ys == unionBy (==) xs ys
union :: Eq a => [a] -> [a] -> [a]
union = unionBy (==)

-- | The 'unionBy' function returns the \"list union\" of two lists,
-- under the given equality predicate.
-- Some examples:
-- 
-- > unionBy (==) "dog" "cow" == "dogcw"
-- > unionBy (==) "moose" "cow" == "moosecw"
-- > unionBy (==) "moose" "woodpecker" == "moosewdpckr"
-- 
-- /O(m n) + O(n^2)/ plus the cost of evaluating the 
-- equality predicate. Laws:
-- 
-- > forall p xs ys . 
-- >   unionBy p xs ys == xs ++ deleteFirstsBy p (nubBy p ys) xs
unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy p xs ys =
  xs ++ deleteFirstsBy p (nubBy p ys) xs

-- | The 'union'' function returns the \"list union\" of
-- two lists. It is a special case of 'unionBy'' with @(==)@
-- as the equality predicate. Laws:
-- 
-- > forall xs ys . union' xs ys == unionBy' (==) xs ys
union' :: Eq a => [a] -> [a] -> [a]
union' = unionBy' (==)

-- | The 'unionBy'' function returns the \"list union\" of
-- two lists, under the given equality predicate, but
-- \"canonicalized\" such that each element appears only
-- once.  /O(m n) + O(m^2) + O(n^2)/ plus the cost of
-- evaluating the equality predicate. Laws:
-- 
-- > forall p xs ys . 
-- >   unionBy' p xs ys == unionBy p (nubBy p xs) (nubBy p ys)
unionBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy' p xs ys =
  unionBy p (nubBy p xs) (nubBy p ys)

-- | The 'intersect' function returns the \"list
-- intersection\" of two lists. It is a special case of
-- 'intersectBy' with @(==)@ as the equality predicate. 
-- /O(m n)/. Laws:
-- 
-- > forall xs ys . intersect xs ys == intersectBy (==) xs ys
intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

-- | The 'intersectBy' function returns the \"list
-- intersection\" of two lists, under the given equality
-- predicate. If the first list contains duplicates, so
-- will the result. Some examples:
-- 
-- > intersectBy (==) [1, 2, 3, 4] [2, 4, 6, 8] == [2, 4]
-- > intersectBy (==) [1, 2, 2, 3, 4] [6, 4, 4, 2] == [2, 2, 4]
-- 
-- /O(m n)/ plus the cost of evaluating the 
-- equality predicate. Laws:
-- 
-- > forall p xs ys . 
-- >   intersectBy p xs ys == filter (\x -> elemBy f x ys) xs
intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy p xs ys =
  filter (\x -> elemBy p x ys) xs

-- | The 'intersect'' function returns the \"list
-- intersection\" of two lists, but with all duplicates
-- removed first. It is a special case of 'intersectBy''
-- with @(==)@ as the equality predicate.  
-- /O(m n) + O(m^2) + O(n^2)/. Laws:
-- 
-- > forall xs ys . intersect xs ys == intersectBy (==) xs ys
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' = intersectBy' (==)

-- | The 'intersectBy'' function returns the \"list
-- intersection\" of two lists, under the given equality
-- predicate, but \"canonicalized\" such that each element
-- appears only once.  /O(m n) + O(m^2) + O(n^2)/ plus the
-- cost of evaluating the equality predicate. Laws:
-- 
-- > forall p xs ys . 
-- >   intersectBy' p xs ys == intersectBy p (nubBy p xs) ys
intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' p xs ys =
  intersectBy p (nubBy p xs) ys

-- | Given two (presumptively) ordered lists, return the
-- ordered merge of the lists, with ties broken to the
-- left. This is a special case of 'mergeBy' that uses
-- 'compare' as the comparison function.  
-- New. /O(m + n)/. Laws:
-- 
-- forall xs ys . merge xs ys == mergeBy compare xs ys
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Given two (presumptively) ordered lists and a
-- comparison function, return the ordered merge of the
-- lists, with ties broken to the left.  
-- New. /O(m + n)/ plus the cost of evaluating the comparison
-- function. Laws:
-- 
-- > forall p xs . mergeBy p [] xs == xs
-- > forall p xs . mergeBy p xs [] == xs
-- > forall p x xs y ys | p x y /= GT . 
-- >   mergeBy p (x : xs) (y : ys) == x : mergeBy p xs (y : ys)
-- > forall p x xs y ys | p x y == GT . 
-- >   mergeBy p (x : xs) (y : ys) == y : mergeBy p (x : xs) ys
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

-- XXX Need to write laws for sorts.

-- | The 'sort' function implements a stable sorting
-- algorithm.  It is a special case of 'sortBy' with
-- comparison function 'compare'. Strict. /O(n)/.
sort :: Ord a => [a] -> [a]
sort = sortBy compare

-- This is an O(n log n) merge sort, not nearly as fast as
-- the magic sort in the standard library. It unfolds
-- nicely, though.

-- | The 'sortBy' function implements a stable sorting
-- algorithm, given a user-supplied comparator
-- function. Strict. /O(n)/.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy c xs0 =
  case fst $ lr f (map (: []) xs0, undefined) of
    [] -> []
    [xs] -> xs
    _ -> error "sort: internal error: incomplete merge"
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

-- | Given a list of 'Ord' values, insert an element @x1@ at the first
-- position such that the next element @x2@ is greater than or equal to @x1@.
-- This function is a special case of 'insertBy' with 'compare' as
-- the comparison function. /O(n)/. Laws:
-- 
-- > forall x0 xs . insert x0 xs == insertBy compare x0 xs
insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

-- | Given a list of 'Ord' values, insert an element @x1@ at the first
-- position such that the next element @x2@ is greater than or equal to @x1@,
-- using the supplied comparison function. Some examples:
-- 
-- > insertBy compare 2 [] == [2]
-- > insertBy compare 2 [1] == [1, 2]
-- > insertBy (comparing fst) (2, 0) [(1, 1), (2, 1), (3, 1)] ==
-- >   [(1, 1), (2, 0), (2, 1), (3, 1)]
-- > insertBy compare 2 [1, 3, 1, 3] == [1, 2, 3, 1, 3]
-- > insertBy compare 2 [3..] == [2..]
-- 
-- This version of insertBy agrees with the code in the
-- standard library, which inserts in the first possible
-- location rather than the last. However, it fails to agree
-- with the documentation for that library. (This is
-- currently bug #7421 in the GHC Trac.)
-- 
-- /O(n)/ plus the cost of evaluating the comparison
-- function. Laws:
-- 
-- > forall c x0 . insertBy c x0 [] == [x0]
-- > forall c x0 x xs | x >= x0 . 
-- >   insertBy c x0 (x : xs) == x0 : x : xs
-- > forall c x0 x xs | x < x0 . 
-- >   insertBy c x0 (x : xs) == x : insertBy c x0 xs
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy c t xs =
  let (l, r) = span ((== GT) . c t) xs in
  l ++ [t] ++ r

-- | Given a list of 'Ord' values, insert an element @x0@
-- into the list at the last position where it is still less
-- than or equal to the next element. If no element is
-- greater than or equal to @x0@, insert it at the end. This
-- function is a special case of 'insertBy'' with 'compare'
-- as the comparison function.  /O(n)/. Laws:
-- 
-- > forall x0 xs . insert' x0 xs == insertBy' compare x0 xs
insert' :: Ord a => a -> [a] -> [a]
insert' = insertBy' compare

-- | Given a list of 'Ord' values, insert an element @x0@
-- into the list at the last position where it is still less
-- than or equal to the next element. If no elements are
-- greater than or equal to @x0@, insert it at the end. Some
-- examples:
-- 
-- > insertBy' compare 2 [] == [2]
-- > insertBy' compare 2 [1] == [1, 2]
-- > insertBy' (comparing fst) (2, 0) [(1, 1), (2, 1), (3, 1)] ==
-- >   [(1, 1), (2, 0), (2, 1), (3, 1)]
-- > insertBy' compare 2 [1, 3, 1, 3] == [1, 3, 1, 2, 3]
-- > insertBy' compare 2 [3..] == _|_
-- 
-- This 'insertBy'' actually follows the contract from the
-- standard library documentation (inasmuch as that contract
-- can be read to specify anything sensible.) It will
-- generally be less efficient than 'insertBy', as it
-- needs to traverse the whole list to check for further
-- insertion points.
-- 
-- New. /O(n)/. Laws:
-- 
-- > forall c x0 . insertBy' c x0 [] == [x0]
-- > forall c x0 x xs | x <= x0 . 
-- >   insertBy c x0 (xs ++ [x]) == xs ++ [x, x0]
-- > forall c x0 x xs | x > x0 . 
-- >   insertBy c x0 (xs ++ [x]) == insertBy c x0 xs ++ [x]
insertBy' :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy' c x0 xs0 =
  let (xs1, xs2) = spanEnd (\x -> x `c` x0 /= LT) xs0 in
  xs1 ++ [x0] ++ xs2
