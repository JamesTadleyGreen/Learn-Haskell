module Lists where

import Data.List

main = print (encode "aaaabccaadeeee")

myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs

myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == myReverse l

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- compress :: [a] -> [a]
-- compress = foldl1 check

-- check :: a -> a -> Maybe a
-- check a b = if a == b then Just a else Nothing

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] -> [(a, Int)]
encode (x:xs) = let (first,rest) = span (==x) xs
                    in (x, length(x:first)) : encode rest
encode [] = []