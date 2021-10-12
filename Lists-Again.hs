import System.Random

main = print (rndSelect "abcdefgh" 3 >>= putStrLn)

insertAt :: a -> [a] -> Int -> [a]
insertAt x l i = let (first, last) = splitAt (i-1) l
    in first ++ [x] ++ last

range :: Int -> Int -> [Int]
range i j = [i..j]

rndSelect :: [a] -> Int -> IO ()
rndSelect l = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]