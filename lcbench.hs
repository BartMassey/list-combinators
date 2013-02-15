{-# LANGUAGE FlexibleInstances #-}
-- Copyright © 2013 Bart Massey
-- Criterion sample benchmarks for Data.List.Combinator

-- C.f. http://stackoverflow.com/questions/12896235

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Criterion.Main
import qualified Data.List.Combinator as LC
import Data.List as L

fbench :: (NFData a, NFData b) 
       => [(String, a)] -> (String, a -> b) -> IO Benchmark
fbench xs (name, f) = do
  bs <- mapM b xs
  return $ bgroup name bs
  where
    b (s, x) = do
      x' <- evaluate $ force x
      return $ bench s (nf f x')

intL :: Int -> Int -> [(String, [Int])]
intL i1 i2 =
  map mk [i1..i2]
  where
    mk i =
      let i10 = 10 ^ i in
      (show i10, [1..i10])

intLL :: Int -> Int -> [(String, [[Int]])]
intLL i1 i2 =
  map mk [i1..i2]
  where
    mk i =
      let i10 = 10 ^ i in
      (show i10, replicate i10 [1..i10])

cf :: (NFData a, NFData b) 
   => String -> [(String, a)] -> [(String, a -> b)] -> IO Benchmark
cf s xs fs = do
  benches <- mapM (fbench xs) fs
  return $ bgroup s benches

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p xs = fst $ partition_ p xs

partition_ :: (a -> Bool) -> [a] -> ([a], [a])
partition_ p xs =
  L.foldr f ([], []) xs
  where
    f x ~(l, r)
      | p x = (x : l, r)
      | otherwise = (l, x : r)

transpose_ :: [[a]] -> [[a]]
transpose_ xss =
  L.unfoldr f xss
  where
    f a 
      | L.null xs  = Nothing
      | otherwise =
          Just (L.foldr g ([], []) xs)
      where
        xs = filter_ (not . L.null) a
        g ~(y : ys) (h, t) = (y : h, ys : t)

main :: IO ()
main = do
  foldrB <- cf "foldr" (intL 4 6) [
    ("L", L.foldr (:) []), 
    ("LC", LC.foldr (:) []) ]
  sumB <- cf "sum" (intL 3 5) [
    ("L", L.sum), 
    ("LC", LC.sum) ]
  sortB <- cf "sort" (intL 3 5) [
    ("L", L.sort), 
    ("LC", LC.sort) ]
  transposeB <- cf "transpose" (intLL 2 3) [
    ("L", L.transpose),  
    ("LC", LC.transpose), 
    ("H", transpose_) ]
  defaultMain [foldrB, sumB, sortB, transposeB]
