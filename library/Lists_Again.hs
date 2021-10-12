module Lists_Again where

import System.Random

main = (rndSelect "abcdefgh" 3 >>= putStrLn)

insertAt :: a -> [a] -> Int -> [a]
insertAt x l i = let (first, last) = splitAt (i-1) l
    in first ++ [x] ++ last

range :: Int -> Int -> [Int]
range i j = [i..j]

rndSelect :: [a] -> Int -> IO [a] -- I don't fully understand this
rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]

-- diff_select :: Int -> Int -> [Int]
-- diff_select i j = rndSelect range i j 