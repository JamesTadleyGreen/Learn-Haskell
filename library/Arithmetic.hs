module Arithmetic where

main = print (isPrime 7)

-- isPrime :: Int -> Int
isPrime i = do
    foldr (==True) False (map (==0) (map (`mod`i) [1..i]))