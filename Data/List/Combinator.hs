-- Copyright © 2012 Bart Massey
-- [This program is licensed under the "BSD License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Data.List.Combinator (
  module Prelude,
  fold,
  foldl,
  foldl',
  foldr,
  append,
  head,
  last,
  tail,
  init,
  null,
  length,
  length',
  map,
  reverse,
  intersperse,
  intercalate,
  transpose,
  subsequences,
  insertions,
  permutations,
  foldl1,
  foldl1',
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
  mapAccumL,
  mapAccumR,
  iterate,
  repeat,
  replicate,
  cycle,
  folds,
  unfold,
  unfold1,
  unfoldr,
  take,
  drop,
  splitAt,
  takeWhile,
  dropWhile,
  dropWhileEnd,
  span,
  break,
  stripPrefix,
  group,
  inits,
  tails,
  isPrefixOf,
  isSuffixOf,
  isInfixOf,
  elem,
  notElem,
  lookup,
  find,
  filter,
  partition,
  index,
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
  mergeBy,
  sortBy,
  insertBy,
  insertBy',
  maximumBy,
  minimumBy,
  genericLength ) where

import Prelude hiding (
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

-- This first bit is Bart Massey, Jamey Sharp and Jules
-- Kongslie's generalized fold.  This fold generalizes a
-- number of things from 'Data.List', including 'foldl' and
-- 'foldr'. It works by allowing `f` to work with both state
-- accumulated from the left and state built up from the
-- right simultaneously.

-- 'fold' is fully lazy if `f` is fully lazy
-- on `l` and `r`, strict if at most one of `l` and `r` is
-- strict, and is bottom if both `l` and `r` are strict.

-- One can think of 'fold' as processing each element of its
-- list input with a function that receives left context
-- calculated from its predecessors and a right context
-- calculated from its successors. As one traverses the list
-- and examines these elements, the function is run to produce
-- these outputs.

-- There is probably a need for versions of these functions
-- strict in the left context: call it fold' .

-- Compare this work with the "bifold" discussed a while back
-- on Haskell-Cafe:
-- 
--    http://haskell.1045720.n5.nabble.com/
--           Bifold-a-simultaneous-foldr-and-foldl-td3285581.html
-- 
-- That fold is identical to this one (up to trivial signature
-- differences). However, I think the subsumption results here
-- are new.  There is some interesting discussion of "Q" from
-- Backus that I would like to absorb someday.

-- | Given a function that accepts an element and a left and
-- right context and produces a new left and right context,
-- and given an initial left and right context and a list,
-- run the function on each element of the list with the
-- appropriate context.
fold :: (x -> (l, r) -> (l, r)) -> (l, r) -> [x] -> (l, r)
fold f lr0 xs0 =
  g lr0 xs0
  where
    g lr [] = lr
    g (l, r) (x : xs) =
      let (l1, r1) = f x (l, r2)
          (l2, r2) = g (l1, r) xs  in
      (l2, r1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = (f l x, r)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = ((f $! l) $! x, r)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b0 =
  snd . fold f' (undefined, b0)
  where
    f' x (l, r) = (l, f x r)

append :: [a] -> [a] -> [a]
xs `append` ys = foldr (:) ys xs

head :: [a] -> a
head (x : _) = x

last :: [a] -> a
last (x : xs) = foldl (\_ y -> y) x xs

tail :: [a] -> [a]
tail (_ : xs) = xs

init :: [a] -> [a]
init xs0 =
  let Just ys = foldr f Nothing xs0 in ys
  where
    f _ Nothing = Just []
    f x (Just xs) = Just (x : xs)

null :: [a] -> Bool
null [] = True
null  _ = False

length :: [a] -> Int
length xs = genericLength xs

-- This type is changed from Data.List to cope with
-- very long lists.
length' :: [a] -> Integer
length' xs = genericLength xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x a -> f x : a) [] xs

reverse :: [a] -> [a]
reverse xs = foldl' (\a x -> x : a) [] xs

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse s xs = tail $ foldr (\x a -> s : x : a) [] xs

-- Taken directly from Data.List.
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

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

subsequences :: [a] -> [[a]]
subsequences xs =
  foldr f [[]] xs
  where
    f x a = a `append` (map (x :) a)

-- Return a list of the lists obtained by inserting x at
-- every position in xs.
insertions :: a -> [a] -> [[a]]
insertions x xs =
  snd $ foldr f ([], [[x]]) xs
  where
    f y (l, r) = (y : l, (x : y : l) : map (y :) r)

permutations :: [a] -> [[a]]
permutations xs =
  foldr f [[]] xs
  where
    f x a = concatMap (insertions x) a

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs 

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x : xs) = foldl' f x xs 

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f xs =
  let Just y = foldr g Nothing xs in y
  where
    g x Nothing = Just x
    g x (Just a) = Just (f a x)

concat :: [[a]] -> [a]
concat xss = foldr (\x a -> append x a) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs =
  foldr (\x a -> f x `append` a) [] xs

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- XXX This should be spine-strict using foldl', but this
-- is not allowed by the Standard (according to the GHC
-- folks), because of some kind of "numbers"?
sum :: (Num a) => [a] -> a
sum = foldl (+) 0

product :: (Num a) => [a] -> a
product = foldl (*) 1

maximum :: Ord a => [a] -> a
maximum = foldl1' max

minimum :: Ord a => [a] -> a
minimum = foldl1' min

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f a0 =
  (a0 :) . snd . mapAccumL g a0
  where
    g a x = let a' = f a x in (a', a')

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z0 =
  snd . foldr f' (z0, [z0])
  where
    f' x (z, rs) =
      let z' = x `f` z in
      (z', z' : rs)

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f xs =
  let Just (_, ys) = foldr f' Nothing xs in ys
  where
    f' x Nothing = Just (x, [x])
    f' x (Just (z, rs)) =
      let z' = x `f` z in
      Just (z', z' : rs)

mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f a0 =
  fold f' (a0, [])
  where
    f' x (l, rs) =
      let (l', r') = f l x in
      (l', r' : rs)

mapAccumR :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f a0 =
  foldr f' (a0, [])
  where
    f' x (a, rs) =
      let (a', r') = f a x in
      (a', r' : rs)

iterate :: (a -> a) -> a -> [a]
iterate f x0 =
  unfoldr g x0
  where
    g x = Just (x, f x)

repeat :: a -> [a]
repeat x = cycle [x]

-- This type is a generalization of the one in Data.List.
replicate :: Integral b => b -> a -> [a]
replicate n = take n . repeat

cycle :: [a] -> [a]
cycle xs =
  let ys = xs `append` ys in ys

-- This generalized fold may be enough to write fold?
folds :: ((l, r) -> (l, Maybe r)) -> (l, r) -> (l, r)
folds f (l, r) =
  let (l1, mr1) = f (l, r2)
      (l2, r2) = folds f (l1, r) in
  case mr1 of
    Nothing -> (l1, r)
    Just r1 -> (l2, r1)

unfold :: ((l, [r]) -> Maybe (r, (l, [r]))) -> (l, [r]) -> (l, [r])
unfold f (l0, rs0) =
  folds g (l0, rs0)
  where
    g (l, rs) =
      case f (l, rs) of
        Just (r, (l', rs')) -> (l', Just (r : rs'))
        Nothing -> (l, Nothing)

-- XXX This avoids using the recursive folds or explicit
-- recursion, but the invisible list cheeziness here is
-- totally bogus.
unfold1 :: ((l, [r]) -> Maybe (r, (l, [r]))) -> (l, [r]) -> (l, [r])
unfold1 f (l0, rs0) =
  fold g (l0, rs0) (repeat undefined)
  where
    g _ (l, rs) =
      case f (l, rs) of
        Just (r, (l', rs')) -> (l', r : rs')
        Nothing -> (l, rs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f a =
  snd $ unfold g (a, [])
  where
    g (l, r) =
      case f l of
        Nothing -> Nothing
        Just (x, l') -> Just (x, (l', r))

-- This type is a generalization of the one in Data.List.
take :: Integral b => b -> [a] -> [a]
take n0 =
  snd . fold take1 (n0, [])
  where
    take1 _ (0, _) = (undefined, [])
    take1 r (n, rs) | n > 0 = (n - 1, r : rs)
    take1 _ _ = error "take with negative count"

-- This type is a generalization of the one in Data.List.
drop :: Integral b => b -> [a] -> [a]
drop n0 =
  snd . fold drop1 (n0, [])
  where
    drop1 r (0, rs) = (0, r : rs)
    drop1 _ (n, rs) | n > 0 = (n - 1, rs)
    drop1 _ _ = error "drop with negative count"

-- This type is a generalization of the one in Data.List.
-- This routine is optimized to be one-pass, which is
-- probably overkill.
splitAt :: Integral b => b -> [a] -> ([a], [a])
splitAt n0 xs =
  let ((_, r), l) = fold f ((0, xs), []) xs in (l, r)
  where
    f x ((n, l), r)
      | n >= n0 || null l = ((n, l), [])
      | otherwise = ((n + 1, tail l), x : r)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = fst $ span p xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = snd $ span p xs

-- Weird new list function, but OK. Definition taken from
-- the standard library and cleaned up a bit.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p =
  foldr f []
  where
    f x a
      | p x && null a = [] 
      | otherwise = x : a

span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs =
  snd $ fold f (True, ([], [])) xs
  where
    f x (b, ~(l, r))
      | b && p x = (True, (x : l, r))
      | otherwise = (False, ([], x : r))

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

stripPrefix  :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix xs ys0 =
  foldl f (Just ys0) xs
  where
    f Nothing _ = Nothing
    f (Just []) _ = Nothing
    f (Just (y : ys)) x
      | x == y = Just ys
      | otherwise = Nothing

group :: Eq a => [a] -> [[a]]
group xs0 =
  unfoldr f xs0
  where
    f [] = Nothing
    f (x : xs) =
      let (g, xs') = span (== x) xs in
      Just (x : g, xs')

-- Adapted from teh standard library.
inits :: [a] -> [[a]]
inits xs =
  foldr f [[]] xs
  where
    f x a = [] : map (x :) a

-- Funny termination. Even has the required strictness
-- property LOL.
tails :: [a] -> [[a]]
tails xs =
  snd $ fold f (xs, [[]]) xs
  where
    f _ (y@(_ : ys), r) = (ys, y : r)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf xs ys =
  case stripPrefix xs ys of
    Nothing -> False
    _ -> True

-- XXX Not so efficient, since it traverses the suffix
-- separately to reverse it, but I don't see how to fix
-- this.
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys =
  case foldr f (Just (reverse ys)) xs of
    Just _ -> True
    Nothing -> False
  where
    f _ Nothing = Nothing
    f _ (Just []) = Nothing
    f x (Just (c : cs))
      | x == c = Just cs
      | otherwise = Nothing


-- XXX Quadratic, but I doubt the standard library
-- is full of Boyer-Moore code.
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)

elem :: Eq a => a -> [a] -> Bool
elem = elemBy (==)

notElem :: Eq a => a -> [a] -> Bool
notElem x0 = not . elem x0
  
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup x0 xs =
  foldr f Nothing xs
  where
    f (xk, xv) a
      | xk == x0 = Just xv
      | otherwise = a

find :: (a -> Bool) -> [a] -> Maybe a
find p xs =
  foldr f Nothing xs
  where
    f x a
      | p x = Just x
      | otherwise = a

filter :: (a -> Bool) -> [a] -> [a]
filter p xs =
  foldr f [] xs
  where
    f x a
      | p x = x : a
      | otherwise = a

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs =
  foldr f ([], []) xs
  where
    f x ~(l, r)
      | p x = (x : l, r)
      | otherwise = (l, x : r)

-- Instead of (!!)
index :: Integral b => b -> [a] -> a
index n xs =
  let Just x = lookup n (zip [0..] xs) in x

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

-- There should be an elemBy. Yes, it's just
-- "any", but still...
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy p x0 xs =  any (p x0) xs

-- OTOH, why is there a notElem? Did we really need that?
notElemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy p x0 = not . elemBy p x0

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

-- This seems to need the general power of folds
-- to get its work done? It's a O(n log n) merge
-- sort, although probably not as fast as the one
-- in the standard library.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy c xs0 =
  case fst $ folds f (map (: []) xs0, undefined) of
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
