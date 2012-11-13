-- Replacement for Data.List using only one recursive
-- function.  At this point this is a work-in-progress, with
-- about 2/3 of the library translated, and a few weird
-- inefficiencies and holes.

fold :: ((l, r) -> x -> (l, r)) -> (l, r) -> [x] -> (l, r)
fold _ lr [] = lr
fold f (l, r) (x : xs) =
  let (l1, r1) = f (l, r2) x
      (l2, r2) = fold f (l1, r) xs in
  (l2, r1)

foldl_ :: (a -> b -> a) -> a -> [b] -> a
foldl_ f a0 = 
  fst . fold f' (a0, undefined)
  where
    f' (l, r) x = (f l x, r)

foldl'_ :: (a -> b -> a) -> a -> [b] -> a
foldl'_ f a0 = 
  fst . fold f' (a0, undefined)
  where
    f' (l, r) x = ((f $! l) $! x, r)

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f b0 = 
  snd . fold f' (undefined, b0)
  where
    f' (l, r) x = (l, f x r)

append_ :: [a] -> [a] -> [a]
xs `append_` ys = foldr_ (:) ys xs

head_ :: [a] -> a
head_ (x : _) = x

last_ :: [a] -> a
last_ (x : xs) = foldl_ (\_ y -> y) x xs

tail_ :: [a] -> [a]
tail_ (_ : xs) = xs

init_ :: [a] -> [a]
-- init_ = reverse_ . tail_ . reverse_
init_ xs =
  let Just ys = foldr_ f Nothing xs in ys
  where
    f _ Nothing = Just []
    f x (Just xs) = Just (x : xs)

null_ :: [a] -> Bool
null_ [] = True
null_  _ = False

-- This type is changed from Data.List to cope with
-- very long lists.
length_ :: [a] -> Integer
length_ xs = foldl'_ (\a _ -> a + 1) 0 xs

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

-- The use of an infinite undefined list to get everything
-- is weird.  This appears to be what it takes to get fold
-- to do the full semantics in one pass.
transpose_ :: [[a]] -> [[a]]
transpose_ xss =
  takeWhile_ (not . null_) $ snd $ fold f (xss, []) (repeat_ undefined)
  where
    f (l, r) _
      | null_ rest = (rest, [first])
      | otherwise = (rest, first : r)
      where
        first = map_ head_ l
        rest = filter_ (not . null_) $ map_ tail_ l

-- This is the recursive transpose. It is actually quite
-- natural, and arguably nicer than the version above.
transpose_1 :: [[a]] -> [[a]]
transpose_1 [] = []
transpose_1 xss =
  map_ head_ xss : transpose_ (filter_ (not . null_) (map_ tail_ xss))

-- This version of transpose is two-pass. This appears to be
-- what it takes to use fold to get the proper semantics,
-- without doing something weird.
transpose_2 :: [[a]] -> [[a]]
transpose_2 xss =
  snd $ fold f (xss, []) [1 .. maximum_ (map_ length_ xss)]
  where
    f (l, r) _ =
      (filter_ (not . null_) $ map_ tail_ l, map_ head_ l : r)

-- This version of transpose will drop elements from the
-- remaining rows of the input as needed to "square up" the
-- "matrix" so that no row is longer than the first, then
-- transpose that.  This appears to be what it takes to use
-- fold instead of general recursion in a single pass
-- without doing something weird.
transpose_3 :: [[a]] -> [[a]]
transpose_3 (xs : xss) =
  snd $ fold f (xss, []) xs
  where
    f (l, r) x =
      (filter_ (not . null_) $ map_ tail_ l, (x : map_ head_ l) : r)

-- This version of transpose will drop elements from the
-- remaining rows of the input as needed to "square up" the
-- "matrix" so that no row is longer than the first, then
-- transpose that.  This version is two-pass.  This appears
-- to be what it takes to use foldr instead of fold.
transpose_4 :: [[a]] -> [[a]]
transpose_4 [] = []
transpose_4 (xs : xss) = 
  reverse $ fst $ foldl_ f ([], xss) xs
  where
    f (ts, us) x = 
      ((x : map_ head_ us) : ts, filter_ (not . null_) $ map_ tail_ us)

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
-- folks).
sum_ :: (Num a) => [a] -> a
sum_ = foldl (+) 0

-- XXX This should be spine-strict using foldl', but this
-- is not allowed by the Standard (according to the GHC
-- folks).
product_ :: (Num a) => [a] -> a
product_ = foldl (*) 1

maximum_ :: Ord a => [a] -> a
maximum_ = foldl1'_ max 

minimum_ :: Ord a => [a] -> a
minimum_ = foldl1'_ min

-- This and others should be rewritten to mapAccumL_.
scanl_ :: (a -> b -> a) -> a -> [b] -> [a]
scanl_ f z0 =
  (z0 :) . snd . fold f' (z0, [])
  where
    f' (z, rs) x =
      let z' = z `f` x in
      (z', z' : rs)

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
    f' (l, rs) x =
      let (l', r') = f l x in
      (l', r' : rs)

mapAccumR_ :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR_ f a0 =
  foldr f' (a0, [])
  where
    f' x (a, rs) =
      let (a', r') = f a x in
      (a', r' : rs)

-- As above, this is kind of grotesque.
iterate_ :: (a -> a) -> a -> [a]
iterate_ f x0 =
  scanl_ f' x0 (repeat_ undefined)
  where
    f' x _ = f x

repeat_ :: a -> [a]
repeat_ x = cycle_ [x]

-- This type is a generalization of the one in Data.List.
replicate_ :: Integral b => b -> a -> [a]
replicate_ n = take_ n . repeat_

cycle_ :: [a] -> [a]
cycle_ xs =
  let ys = xs `append_` ys in ys

-- The bogus invisible list cheeziness again.
unfoldr_ :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr_ f a = 
  snd $ fold g (a, []) (repeat_ undefined)
  where
    g (l, r) _ =
      case f l of
        Just (x, l') -> (l', x : r)
        Nothing -> (l, [])

-- This type is a generalization of the one in Data.List.
take_ :: Integral b => b -> [a] -> [a]
take_ n0 =
  snd . fold take1 (n0, [])
  where
    take1 (0, _) _ = (undefined, [])
    take1 (n, rs) r | n > 0 = (n - 1, r : rs)
    take1 _ _ = error "take with negative count"

-- This type is a generalization of the one in Data.List.
drop_ :: Integral b => b -> [a] -> [a]
drop_ n0 =
  snd . fold drop1 (n0, [])
  where
    drop1 (0, rs) r = (0, r : rs)
    drop1 (n, rs) _ | n > 0 = (n - 1, rs)
    drop1 _ _ = error "drop with negative count"

-- This type is a generalization of the one in Data.List.
-- This routine is optimized to be one-pass, which is
-- probably overkill.
splitAt_ :: Integral b => b -> [a] -> ([a], [a])
splitAt_ n0 xs =
  let ((_, r), l) = fold f ((0, xs), []) xs in (l, r)
  where
    f ((n, l), r) x
      | n >= n0 || null_ l = ((n, l), [])
      | otherwise = ((n + 1, tail_ l), x : r)

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ p xs =
  foldr f [] xs
  where
    f x a
      | p x = x : a
      | otherwise = []

-- XXX Is this efficient? It appears to be lazy...
dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ p xs =
  snd $ fold f (True, []) xs
  where
    f (l, r) x
      | l && p x = (True, r)
      | otherwise = (False, x : r)

-- Weird new list function, but OK. Definition taken from
-- the standard library and cleaned up a bit.
dropWhileEnd_ :: (a -> Bool) -> [a] -> [a]
dropWhileEnd_ p =
  foldr_ f []
  where
    f x a
      | p x && null_ a = [] 
      | otherwise = x : a

-- This routine is a slightly-adapted splitAt_.
span_ :: (a -> Bool) -> [a] -> ([a], [a])
span_ p xs =
  let ((_, r), l) = fold f ((True, xs), []) xs in (l, r)
  where
    f ((ok, l), r) x
      | ok && p x && not (null l) = ((ok, tail_ l), x : r)
      | otherwise = ((False, l), [])

break_ :: (a -> Bool) -> [a] -> ([a], [a])
break_ p = span_ (not . p)

stripPrefix_  :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix_ xs ys = 
  foldl_ f (Just ys) xs
  where
    f Nothing x = Nothing
    f (Just []) x = Nothing
    f (Just (y : ys)) x
      | x == y = Just ys
      | otherwise = Nothing

-- XXX The strictness of this is no good: it tries to read
-- out the rightmost group before returning the left one. I
-- don't see how to fix this right now.
group_ :: Eq a => [a] -> [[a]]
group_ [] = []
group_ xs =
  foldr_ f [] xs
  where
    f x [] = [[x]]
    f x ([] : gs) = [x] : gs
    f x (g0@(g : _) : gs)
      | x == g = (x : g0) : gs
      | otherwise = [x] : g0 : gs

-- Adapted from teh standard library.
inits_ :: [a] -> [[a]]
inits_ xs =
  foldr_ f [[]] xs
  where
    f x a = [] : map_ (x :) a

-- XXX More ickiness for termination. Even
-- has the required strictness property LOL.
tails_ :: [a] -> [[a]]
tails_ xs =
  snd $ fold f (xs, [[]]) xs
  where
    f (y@(_ : ys), r) _ = (ys, y : r)


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
    Just x -> True
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
elem_ x0 xs = 
  foldr_ f False xs
  where
    f x a = x == x0 || a

notElem_ :: Eq a => a -> [a] -> Bool
notElem_ x0 xs =
  foldr_ f True xs
  where
    f x a = x /= x0 && a
  
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
    f x (l, r)
      | p x = (x : l, r)
      | otherwise = (l, x : r)

-- Instead of (!!)
elemAt_ :: Integral b => b -> [a] -> a
elemAt_ n xs =
  let Just x = lookup_ n (zip_ [1..] xs) in x

-- More infinite undefines nonsense.
zip_ :: [a] -> [b] -> [(a, b)]
zip_ xs1 xs2 =
  snd $ fold f ((xs1, xs2), []) (repeat_ undefined)
  where
    f :: (([a], [b]), [(a, b)]) -> c -> (([a], [b]), [(a, b)])
    f ((l1 : l1s, l2 : l2s), r) _ = ((l1s, l2s), (l1, l2) : r)
    f _ _ = (undefined, [])

nub_ :: Eq a => [a] -> [a]
nub_ =
  snd . fold nub1 ([], [])
  where
    nub1 (l, rs) x
      | x `elem` l = (l, rs)
      | otherwise = (x : l, x : rs)

