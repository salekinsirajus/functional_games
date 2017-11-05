import System.Random
import Data.Time.Clock.POSIX
import System.IO.Unsafe

-- This is using unsafePerformIO, since we will be using
-- the random shuffle for using things that might not 
-- involve IO. This is not the recommended way, but it gives
-- us a random number to use.
integralTime = round (unsafePerformIO getPOSIXTime) :: Int

getRandom :: Int -> Int
getRandom n = integralTime `mod` n


shuffle :: [a] -> [a]
shuffle xs 
    | length xs < 2 = xs
    | otherwise = do
        let n = getRandom (length xs -1)
        [xs !! n] ++ shuffle (take n xs ++ drop (n+1) xs)

main = do
    print $ shuffle [1,3,4,5,6,7]
