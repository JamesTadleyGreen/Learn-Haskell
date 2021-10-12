module Lists_Continued where

import Data.List
import Data.Maybe
import GHC.Prelude (Show)

main = print (removeAt 2 "abcd")

data Occourance a = Single a | Multiple Int a deriving (Show)

encodeModified ::  Eq a => [a] -> [Occourance a]
encodeModified xs = map encodeOccourance (group xs)
    where encodeOccourance [x] = Single x
          encodeOccourance l = Multiple (length l) (head l)

decodeModified :: [Occourance a] -> [a]
decodeModified = concatMap decodeOccourance
    where decodeOccourance (Single x) = [x]
          decodeOccourance (Multiple i x) = replicate i x

encodeDirect :: Eq a => [a] -> [Occourance a]
encodeDirect (x:xs) = let (first,rest) = span (==x) xs
                    in encodeOccourance x (length(x:first)) : encodeDirect rest
                    where encodeOccourance x 1 = Single x
                          encodeOccourance x i = Multiple i x
encodeDirect [] = []

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

repli :: Int -> [a] -> [a]
repli i = concatMap (replicate i)

dropEvery :: [a] -> Int -> [a]
dropEvery _ 1 = []
dropEvery [] _ = []
dropEvery [x] _ = [x]
dropEvery l i = let (first, rest) = splitAt i l
    in init first ++ dropEvery rest i

split :: [a] -> Int -> ([a],[a])
split l i = ([x | (x, c) <- zip l [1,2..], c <= i], [x | (x, c) <- zip l [1,2..], c > i])

slice :: [a] -> Int -> Int -> [a]
slice l 1 j = fst (split l j)
slice (x:xs) i j = slice xs (i-1) (j-1)

rotate :: [a] -> Int -> [a]
rotate l i | i>0 = snd x ++ fst x where x = splitAt i l
rotate l i = snd x ++ fst x where x = splitAt (length l + i) l

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt i (x:xs) = (fst (removeAt (i-1) xs), x:snd (removeAt (i-1) xs))
    -- where
    --     ys = removeHelper xs
    --     y  = xs
    --     removeHelper 0 xs = tail xs
    --     removeHelper i (x:xs) = x : removeHelper (i-1) xs