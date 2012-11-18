-- Copyright © 2012 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Data.List.Combinator (
  fold,
  foldl_,
  foldl'_,
  foldr_,
  init_,
  transpose_,
  subsequences_,
  insertions,
  permutations_,
  foldr1_,
  concatMap_,
  scanl_,
  scanr_,
  scanr1_,
  mapAccumL_,
  mapAccumR_,
  iterate_,
  cycle_,
  folds,
  unfold,
  unfold1,
  unfoldr_,
  take_,
  drop_,
  splitAt_,
  takeWhile_,
  dropWhile_,
  dropWhileEnd_,
  stripPrefix_,
  group_,
  inits_,
  tails_,
  isPrefixOf_,
  isSuffixOf_,
  lookup_,
  find_,
  filter_,
  partition_,
  index_,
  zipWith_,
  unzip_,
  unzip3_,
  unzip4_,
  lines_,
  words_,
  nubBy_,
  deleteBy_,
  listDiffBy_,
  listDiffBy'_,
  unionBy_,
  unionBy'_,
  intersectBy_,
  intersectBy'_,
  mergeBy,
  sortBy_,
  insertBy_,
  insertBy'_,
  maximumBy_,
  minimumBy_ ) where

import Data.Char (isSpace)

fold :: (x -> (l, r) -> (l, r)) -> (l, r) -> [x] -> (l, r)
fold f lr0 xs0 =
  g lr0 xs0
  where
    g lr [] = lr
    g (l, r) (x : xs) =
      let (l1, r1) = f x (l, r2)
          (l2, r2) = g (l1, r) xs  in
      (l2, r1)

foldl_ :: (a -> b -> a) -> a -> [b] -> a
foldl_ f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = (f l x, r)

foldl'_ :: (a -> b -> a) -> a -> [b] -> a
foldl'_ f a0 =
  fst . fold f' (a0, undefined)
  where
    f' x (l, r) = ((f $! l) $! x, r)

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f b0 =
  snd . fold f' (undefined, b0)
  where
    f' x (l, r) = (l, f x r)

append_ :: [a] -> [a] -> [a]
xs `append_` ys = foldr_ (:) ys xs

head_ :: [a] -> a
head_ (x : _) = x

last_ :: [a] -> a
last_ (x : xs) = foldl_ (\_ y -> y) x xs

tail_ :: [a] -> [a]
tail_ (_ : xs) = xs

init_ :: [a] -> [a]
init_ xs0 =
  let Just ys = foldr_ f Nothing xs0 in ys
  where
    f _ Nothing = Just []
    f x (Just xs) = Just (x : xs)

null_ :: [a] -> Bool
null_ [] = True
null_  _ = False

length_ :: [a] -> Int
length_ xs = genericLength xs

-- This type is changed from Data.List to cope with
-- very long lists.
length'_ :: [a] -> Integer
length'_ xs = genericLength xs

map_ :: (a -> b) -> [a] -> [b]
map_ f xs = foldr_ (\x a -> f x : a) [] xs

reverse_ :: [a] -> [a]
reverse_ xs = foldl'_ (\a x -> x : a) [] xs

intersperse_ :: a -> [a] -> [a]
intersperse_ _ [] = []
intersperse_ s xs = tail_ $ foldr_ (\x a -> s : x : a) [] xs

-- Taken directly from Data.List.
intercalate_ :: [a] -> [[a]] -> [a]
intercalate_ xs xss = concat_ (intersperse_ xs xss)

transpose_ :: [[a]] -> [[a]]
transpose_ xss =
  unfoldr_ f xss
  where
    f a 
      | null_ xs  = Nothing
      | otherwise =
          Just (foldr_ g ([], []) xs)
      where
        xs = filter_ (not . null_) a
        g (y : ys) (h, t) = (y : h, ys : t)

subsequences_ :: [a] -> [[a]]
subsequences_ xs =
  foldr_ f [[]] xs
  where
    f x a = a `append_` (map_ (x :) a)

-- Return a list of the lists obtained by inserting x at
-- every position in xs.
insertions :: a -> [a] -> [[a]]
insertions x xs =
  snd $ foldr_ f ([], [[x]]) xs
  where
    f y (l, r) = (y : l, (x : y : l) : map_ (y :) r)

permutations_ :: [a] -> [[a]]
permutations_ xs =
  foldr_ f [[]] xs
  where
    f x a = concatMap_ (insertions x) a

foldl1_ :: (a -> a -> a) -> [a] -> a
foldl1_ f (x : xs) = foldl_ f x xs 

foldl1'_ :: (a -> a -> a) -> [a] -> a
foldl1'_ f (x : xs) = foldl'_ f x xs 

foldr1_ :: (a -> a -> a) -> [a] -> a
foldr1_ f xs =
  let Just y = foldr_ g Nothing xs in y
  where
    g x Nothing = Just x
    g x (Just a) = Just (f a x)

concat_ :: [[a]] -> [a]
concat_ xss = foldr_ (\x a -> append_ x a) [] xss

concatMap_ :: (a -> [b]) -> [a] -> [b]
concatMap_ f xs =
  foldr_ (\x a -> f x `append_` a) [] xs

and_ :: [Bool] -> Bool
and_ = foldr_ (&&) True

or_ :: [Bool] -> Bool
or_ = foldr_ (||) False

any_ :: (a -> Bool) -> [a] -> Bool
any_ p = or_ . map_ p

all_ :: (a -> Bool) -> [a] -> Bool
all_ p = and_ . map_ p

-- XXX This should be spine-strict using foldl', but this
-- is not allowed by the Standard (according to the GHC
-- folks), because of some kind of "numbers"?
sum_ :: (Num a) => [a] -> a
sum_ = foldl (+) 0

product_ :: (Num a) => [a] -> a
product_ = foldl (*) 1

maximum_ :: Ord a => [a] -> a
maximum_ = foldl1'_ max

minimum_ :: Ord a => [a] -> a
minimum_ = foldl1'_ min

scanl_ :: (a -> b -> a) -> a -> [b] -> [a]
scanl_ f a0 =
  (a0 :) . snd . mapAccumL_ g a0
  where
    g a x = let a' = f a x in (a', a')

scanl1_ :: (a -> a -> a) -> [a] -> [a]
scanl1_ f (x : xs) = scanl_ f x xs

scanr_ :: (a -> b -> b) -> b -> [a] -> [b]
scanr_ f z0 =
  snd . foldr f' (z0, [z0])
  where
    f' x (z, rs) =
      let z' = x `f` z in
      (z', z' : rs)

scanr1_ :: (a -> a -> a) -> [a] -> [a]
scanr1_ f xs =
  let Just (_, ys) = foldr f' Nothing xs in ys
  where
    f' x Nothing = Just (x, [x])
    f' x (Just (z, rs)) =
      let z' = x `f` z in
      Just (z', z' : rs)

mapAccumL_ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL_ f a0 =
  fold f' (a0, [])
  where
    f' x (l, rs) =
      let (l', r') = f l x in
      (l', r' : rs)

mapAccumR_ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR_ f a0 =
  foldr f' (a0, [])
  where
    f' x (a, rs) =
      let (a', r') = f a x in
      (a', r' : rs)

iterate_ :: (a -> a) -> a -> [a]
iterate_ f x0 =
  unfoldr_ g x0
  where
    g x = Just (x, f x)

repeat_ :: a -> [a]
repeat_ x = cycle_ [x]

-- This type is a generalization of the one in Data.List.
replicate_ :: Integral b => b -> a -> [a]
replicate_ n = take_ n . repeat_

cycle_ :: [a] -> [a]
cycle_ xs =
  let ys = xs `append_` ys in ys

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
  fold g (l0, rs0) (repeat_ undefined)
  where
    g _ (l, rs) =
      case f (l, rs) of
        Just (r, (l', rs')) -> (l', r : rs')
        Nothing -> (l, rs)

unfoldr_ :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr_ f a =
  snd $ unfold g (a, [])
  where
    g (l, r) =
      case f l of
        Nothing -> Nothing
        Just (x, l') -> Just (x, (l', r))

-- This type is a generalization of the one in Data.List.
take_ :: Integral b => b -> [a] -> [a]
take_ n0 =
  snd . fold take1 (n0, [])
  where
    take1 _ (0, _) = (undefined, [])
    take1 r (n, rs) | n > 0 = (n - 1, r : rs)
    take1 _ _ = error "take with negative count"

-- This type is a generalization of the one in Data.List.
drop_ :: Integral b => b -> [a] -> [a]
drop_ n0 =
  snd . fold drop1 (n0, [])
  where
    drop1 r (0, rs) = (0, r : rs)
    drop1 _ (n, rs) | n > 0 = (n - 1, rs)
    drop1 _ _ = error "drop with negative count"

-- This type is a generalization of the one in Data.List.
-- This routine is optimized to be one-pass, which is
-- probably overkill.
splitAt_ :: Integral b => b -> [a] -> ([a], [a])
splitAt_ n0 xs =
  let ((_, r), l) = fold f ((0, xs), []) xs in (l, r)
  where
    f x ((n, l), r)
      | n >= n0 || null_ l = ((n, l), [])
      | otherwise = ((n + 1, tail_ l), x : r)

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ p xs = fst $ span_ p xs

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ p xs = snd $ span_ p xs

-- Weird new list function, but OK. Definition taken from
-- the standard library and cleaned up a bit.
dropWhileEnd_ :: (a -> Bool) -> [a] -> [a]
dropWhileEnd_ p =
  foldr_ f []
  where
    f x a
      | p x && null_ a = [] 
      | otherwise = x : a

span_ :: (a -> Bool) -> [a] -> ([a], [a])
span_ p xs =
  foldr_ f ([], []) xs
  where
    f x ~(l, r)
      | p x = (x : l, r)
      | otherwise = ([], x : r)

break_ :: (a -> Bool) -> [a] -> ([a], [a])
break_ p = span_ (not . p)

stripPrefix_  :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix_ xs ys0 =
  foldl_ f (Just ys0) xs
  where
    f Nothing _ = Nothing
    f (Just []) _ = Nothing
    f (Just (y : ys)) x
      | x == y = Just ys
      | otherwise = Nothing

group_ :: Eq a => [a] -> [[a]]
group_ xs0 =
  unfoldr_ f xs0
  where
    f [] = Nothing
    f (x : xs) =
      let (g, xs') = span_ (== x) xs in
      Just (x : g, xs')

-- Adapted from teh standard library.
inits_ :: [a] -> [[a]]
inits_ xs =
  foldr_ f [[]] xs
  where
    f x a = [] : map_ (x :) a

-- Funny termination. Even has the required strictness
-- property LOL.
tails_ :: [a] -> [[a]]
tails_ xs =
  snd $ fold f (xs, [[]]) xs
  where
    f _ (y@(_ : ys), r) = (ys, y : r)

isPrefixOf_ :: Eq a => [a] -> [a] -> Bool
isPrefixOf_ xs ys =
  case stripPrefix_ xs ys of
    Nothing -> False
    _ -> True

-- XXX Not so efficient, since it traverses the suffix
-- separately to reverse it, but I don't see how to fix
-- this.
isSuffixOf_ :: Eq a => [a] -> [a] -> Bool
isSuffixOf_ xs ys =
  case foldr_ f (Just (reverse ys)) xs of
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
isInfixOf_ :: Eq a => [a] -> [a] -> Bool
isInfixOf_ xs ys = any_ (isPrefixOf_ xs) (tails_ ys)

elem_ :: Eq a => a -> [a] -> Bool
elem_ = elemBy_ (==)

notElem_ :: Eq a => a -> [a] -> Bool
notElem_ x0 = not . elem_ x0
  
lookup_ :: Eq a => a -> [(a, b)] -> Maybe b
lookup_ x0 xs =
  foldr_ f Nothing xs
  where
    f (xk, xv) a
      | xk == x0 = Just xv
      | otherwise = a

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ p xs =
  foldr_ f Nothing xs
  where
    f x a
      | p x = Just x
      | otherwise = a

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p xs =
  foldr_ f [] xs
  where
    f x a
      | p x = x : a
      | otherwise = a

partition_ :: (a -> Bool) -> [a] -> ([a], [a])
partition_ p xs =
  foldr_ f ([], []) xs
  where
    f x ~(l, r)
      | p x = (x : l, r)
      | otherwise = (l, x : r)

-- Instead of (!!)
index_ :: Integral b => b -> [a] -> a
index_ n xs =
  let Just x = lookup_ n (zip_ [0..] xs) in x

-- This idea comes from the standard library.

zip_ :: [a] -> [b] -> [(a, b)]
zip_ =  zipWith_ (,)

zip3_ :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3_ =  zipWith3_ (,,)

zip4_ :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4_ =  zipWith4_ (,,,)

-- No, I don't believe anyone uses higher-arity zips, so there.

zipWith_ :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_ f xs1 xs2 =
  unfoldr_ g (xs1, xs2)
  where
    g (l1 : l1s, l2 : l2s) = Just (f l1 l2, (l1s, l2s))
    g _ = Nothing

zipWith3_ :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3_ f xs1 xs2 = zipWith_ ($) (zipWith_ f xs1 xs2)

zipWith4_ :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4_ f xs1 xs2 xs3 = zipWith_ ($) (zipWith3_ f xs1 xs2 xs3)

unzip_ :: [(a, b)] -> ([a], [b])
unzip_ =
  foldr_ f ([], [])
  where
    f (a, b) (as, bs) = (a : as, b : bs)

unzip3_ :: [(a, b, c)] -> ([a], [b], [c])
unzip3_ =
  foldr_ f ([], [], [])
  where
    f (a, b, c) (as, bs, cs) = (a : as, b : bs, c : cs)

unzip4_ :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4_ =
  foldr_ f ([], [], [], [])
  where
    f (a, b, c, d) (as, bs, cs, ds) = (a : as, b : bs, c : cs, d : ds)

lines_ :: String -> [String]
lines_ "" = []
lines_ s =
  unfoldr_ f (Just s)
  where
    f Nothing = Nothing
    f (Just cs) =
      let (l, r) = break_ (== '\n') cs in
      case r of
        ('\n' : ls@(_ : _)) -> Just (l, Just ls)
        _ -> Just (l, Nothing)

words_ :: String -> [String]
words_ s =
  unfoldr_ f (Just s)
  where
    f Nothing = Nothing
    f (Just cs) =
      let (_, r) = span_ isSpace cs in
      let (l, r') = break_ isSpace r in
      case r' of
        "" -> 
          case l of
            "" -> Nothing
            _ -> Just (l, Nothing)
        _ -> Just (l, Just r')

unlines_ :: [String] -> String
unlines_ [] = ""
unlines_ ls = intercalate_ "\n" ls ++ "\n"

unwords_ :: [String] -> String
unwords_ ws = intercalate_ " " ws

nub_ :: Eq a => [a] -> [a]
nub_ = nubBy_ (==)

delete_ :: Eq a => a -> [a] -> [a]
delete_ = deleteBy_ (==)

-- Instead of (\\)
listDiff_ :: Eq a => [a] -> [a] -> [a]
listDiff_ = listDiffBy_ (==)

listDiff'_ :: Eq a => [a] -> [a] -> [a]
listDiff'_ = listDiffBy'_ (==)

union_ :: Eq a => [a] -> [a] -> [a]
union_ = unionBy_ (==)

union'_ :: Eq a => [a] -> [a] -> [a]
union'_ = unionBy'_ (==)

intersect_ :: Eq a => [a] -> [a] -> [a]
intersect_ = intersectBy_ (==)

intersect'_ :: Eq a => [a] -> [a] -> [a]
intersect'_ = intersectBy'_ (==)

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

sort_ :: Ord a => [a] -> [a]
sort_ = sortBy_ compare

insert_ :: Ord a => a -> [a] -> [a]
insert_ = insertBy_ compare

insert'_ :: Ord a => a -> [a] -> [a]
insert'_ = insertBy'_ compare

-- There should be an elemBy. Yes, it's just
-- "any", but still...
elemBy_ :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy_ p x0 xs =  any (p x0) xs

-- OTOH, why is there a notElem? Did we really need that?
notElemBy_ :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy_ p x0 = not . elemBy_ p x0

nubBy_ :: (a -> a -> Bool) -> [a] -> [a]
nubBy_ f =
  snd . fold g ([], [])
  where
    g x (l, rs)
      | elemBy_ f x l = (l, rs)
      | otherwise = (x : l, x : rs)

deleteBy_ :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy_ p t es =
  snd $ fold f (True, []) es
  where
    f x (l, r)
      | l && p x t = (False, r)
      | otherwise = (l, x : r)

listDiffBy_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listDiffBy_ f xs ys =
  foldl_ (flip (deleteBy_ f)) xs ys

-- This definition of listDiffBy makes the result canonical
-- on all inputs.
listDiffBy'_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listDiffBy'_ f xs ys =
  filter (\x -> notElemBy_ f x (nubBy_ f ys)) (nubBy_ f xs)

unionBy_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy_ f xs ys =
  xs ++ listDiffBy_ f (nubBy_ f ys) xs

-- The standard definition of unionBy is maximally lazy:
-- this one makes the result canonical on all inputs.
unionBy'_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy'_ f xs ys =
  let xs' = nubBy_ f xs in
  xs' ++ listDiffBy_ f (nubBy_ f ys) xs'

intersectBy_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy_ f xs ys =
  filter_ (\x -> elemBy_ f x ys) xs

-- This definition of intersectBy makes the result canonical
-- on all inputs.
intersectBy'_ :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy'_ f xs ys =
  filter_ (\x -> elemBy_ f x (nubBy_ f ys)) (nubBy_ f xs)


-- This should be in the standard library anyhow.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy c xs1 xs2 =
  unfoldr_ f (xs1, xs2)
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
sortBy_ :: (a -> a -> Ordering) -> [a] -> [a]
sortBy_ c xs0 =
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
          unfoldr_ g xss
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
insertBy_ :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy_ c t xs0 =
  let (xs1, xs2) = span_ (\x -> t `c` x /= LT) xs0 in
  let xs2' =
        case foldr_ f (Left []) xs2 of
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
insertBy'_ :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy'_ c t xs =
  let (l, r) = span_ ((== GT) . c t) xs in
  l ++ [t] ++ r

maximumBy_ :: (a -> a -> Ordering) -> [a] -> a
maximumBy_ c xs =
  foldl1'_ f xs
  where
    x1 `f` x2
      | x1 `c` x2 == LT = x2
      | otherwise = x1

minimumBy_ :: (a -> a -> Ordering) -> [a] -> a
minimumBy_ c xs =
  foldl1'_ f xs
  where
    x1 `f` x2
      | x1 `c` x2 == GT = x2
      | otherwise = x1

-- The rest of the functions are already generic,
-- because why not? Length not so much, since one
-- tends to use it to anchor type constraints.
genericLength :: Num a => [b] -> a
genericLength xs = foldl'_ (\a _ -> a + 1) 0 xs
