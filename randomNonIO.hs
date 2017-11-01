import System.Random

getRandom :: Int -> Int
getRandom n = 1234435 `mod` n


shuffle :: [a] -> [a]
--shuffle [] = []
shuffle xs 
    | length xs < 2 = xs
    | otherwise = do
        let n = getRandom (length xs -1)
        [xs !! n] ++ shuffle (take n xs ++ drop (n+1) xs)

main = do
    print $ shuffle [1,3,4,5,6,7]
