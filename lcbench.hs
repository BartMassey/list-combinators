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
       => String -> [(String, a)] -> (a -> b) -> IO Benchmark
fbench name xs f = do
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
   => String -> [(String, a)] -> (a -> b) -> (a -> b) -> IO Benchmark
cf s xs lf lcf = do
  lbench <- fbench "L" xs lf
  rbench <- fbench "LC" xs lcf
  return $ bgroup s [lbench, rbench]

main :: IO ()
main = do
  foldrB <- cf "foldr" (intL 4 6) (L.foldr (:) []) (LC.foldr (:) [])
  sumB <- cf "sum" (intL 3 5) L.sum LC.sum
  transposeB <- cf "transpose" (intLL 2 3) L.transpose LC.transpose
  sortB <- cf "sort" (intL 3 5) L.sort LC.sort
  defaultMain [foldrB, sumB, transposeB, sortB]
