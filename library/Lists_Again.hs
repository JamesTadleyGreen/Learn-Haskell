module Lists_Again where

import System.Random
import Data.List

-- main = (rndPermu "abcdef" >>= print)

insertAt :: a -> [a] -> Int -> [a]
insertAt x l i = let (first, last) = splitAt (i-1) l
    in first ++ [x] ++ last

range :: Int -> Int -> [Int]
range i j = [i..j]

rndSelect :: [a] -> Int -> IO [a] -- I don't fully understand this
rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]

diff_select :: Int -> Int -> IO [Int] -- Not distinct
diff_select i j = rndSelect (range 1 j) i 

-- rnd_permu :: [a] -> [([Int],a)]
-- rndPermu xs = zip l xs
    -- where
        -- l = take (length xs) . nub . randomRs (1, length xs) <$> getStdGen