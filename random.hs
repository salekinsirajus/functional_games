-- This piece of code works with a list of tuples
-- so it's safe to say that it will work with the
-- PalyingCard type that we have as well.
--
import System.Random

shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

