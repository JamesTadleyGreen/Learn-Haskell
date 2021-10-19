module Arithmetic where

import Data.List
import Data.Maybe

main = print (primeFactors 315)

isPrime :: Int -> Bool
isPrime i =
  -- Slow as we check up to the number where we could check up to the sqrt or just 6k\pm 1
  isNothing (find (== 0) (tail (init (map (i `mod`) [1 .. i]))))

myGCD :: Int -> Int -> Int
myGCD i 0 = abs i
myGCD i j = myGCD j (i - (i `div` j) * j) -- This is mod you idiot

coprime :: Int -> Int -> Bool
coprime i j = myGCD i j == 1

totient :: Int -> Int
totient i = foldr (\x -> if coprime i x then (+ 1) else (+ 0)) 0 [1 .. i]

primeFactors :: Int -> [Int]
primeFactors x
  | isPrime x = [x]
  | otherwise = sp : primeFactors (x `div` sp)
  where sp = head [y | y <- [2..], not (coprime y x)] -- This is inefficient

  primeFactorsMult :: Int -> [(Int, Int)]