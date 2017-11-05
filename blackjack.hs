{-
Victor Zuniga
Yuki Adams
Shaown Salekin
-}

import System.Random
import Control.Monad
import System.IO
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

data Suit = Clubs|Diamonds|Hearts|Spades
            deriving (Show, Eq, Ord, Enum, Bounded)

data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
             deriving (Show, Eq, Ord, Enum, Bounded)

data PlayingCards = Card {val::Value,su::Suit}
                    deriving (Show, Eq)

allSuits = [(minBound::Suit) ..]
allValues = [(minBound::Value) ..]
buildDeck :: [Value] -> [Suit] -> [PlayingCards]
buildDeck a b = [Card x y | x <- a, y <- b]
deck = shuffle (buildDeck allValues allSuits)

--Games

main = do
    blackjack deck

--Blackjack

blackjack :: [PlayingCards] -> IO () --This function sets up the dealer's and player's hands, then calls hitOrStay to complete the game
blackjack d = do
    let dHand = []
    let disDHand = dHand ++ (take 2 d)
    let dTemp = drop 2 d
    let d = dTemp
    let hidDHand = head disDHand
    putStrLn ("Dealer's Hand: " ++ show (val hidDHand,su hidDHand) ++ " ****")
    let pHand = []
    let disPHand = pHand ++ (take 2 d)
    let dTemp = drop 2 d
    let d = dTemp
    putStrLn ("Your Hand: " ++ show [(val x,su x) | x <- disPHand])
    when (total disPHand == 21) $ do
        dHitOrStay d disDHand disPHand
    hitOrStay d disDHand disPHand

hitOrStay :: [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> IO () --This function handles the player's hit or stay choices
hitOrStay d disDHand disPHand = do
    putStrLn "Would you like to hit or stay? "
    choice <- getLine
    when (choice /= "hit" && choice /= "stay") $ do
        putStrLn "Invalid - Please enther 'hit' or 'stay'"
        hitOrStay d disDHand disPHand
    when (choice == "hit") $ do
        let pHTemp = disPHand ++ (take 1 d)
        let dTemp = drop 1 d
        let disPHand = pHTemp
        let d = dTemp
        putStrLn ("Your Hand: " ++ (show [(val x, su x) | x <- disPHand]))
        when (total disPHand <= 21) $ do
            hitOrStay d disDHand disPHand
        when (total disPHand > 21) $ do
            endgame "1" disDHand disPHand
    when (choice == "stay") $ do
        putStrLn "The dealer will go now"
        dHitOrStay d disDHand disPHand

dHitOrStay :: [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> IO ()
dHitOrStay d disDHand disPHand= do
    when (total disDHand >= 17 && total disDHand <= 21) $ do
        findWinner disDHand disPHand
    when (total disDHand > 21) $ do
        endgame "2" disDHand disPHand
    when (total disDHand < 17) $ do
        let dHTemp = disDHand ++ (take 1 d)
        let dTemp = drop 1 d
        let disDHand = dHTemp
        let d = dTemp
        dHitOrStay d disDHand disPHand

findWinner :: [PlayingCards] -> [PlayingCards] -> IO ()
findWinner disDHand disPHand = do
    when (total disDHand > total disPHand) $ do
        endgame "3" disDHand disPHand
    when (total disDHand < total disPHand) $ do
        endgame "2" disDHand disPHand
    when (total disDHand == total disPHand) $ do
        endgame "4" disDHand disPHand

total :: [PlayingCards] -> Int --calculate the total of cards in a hand
total d = totalValue (moveAces [valToInt x | x <- d] 0 (acesCounter [valToInt x | x <- d] 0)) 0

valToInt :: PlayingCards -> Int --convert card value to a number value
valToInt c --card
    |val c == Two   = 2
    |val c == Three = 3
    |val c == Four  = 4
    |val c == Five  = 5
    |val c == Six   = 6
    |val c == Seven = 7
    |val c == Eight = 8
    |val c == Nine  = 9
    |val c == Ten   = 10
    |val c == Jack  = 10
    |val c == Queen = 10
    |val c == King  = 10
    |val c == Ace   = 1

moveAces :: [Int] -> Int -> Int -> [Int] --move aces to end of hand (recursion doesn't work perfectly yet)
moveAces v w x --list of card vals, pointing index, number of aces
    |(w == ((length v) - x))        = v
    |((v!!w == 1) || (v!!w == 11)) = moveAces ((take w v) ++ (drop (w + 1) v) ++ [v!!w]) w x
    |((v!!w /= 1) || (v!!w == 11)) = moveAces v (w + 1) x

totalValue :: [Int] -> Int -> Int --calculate the total of cards in a hand
totalValue v t --list of card vals, total counter
    |(length v == 0)                  = 0
    |(length v == 1) && (head v == 1) = decideAce t
    |(length v == 1) && (head v /= 1) = t + head v
    |otherwise                        = totalValue (tail v) (t + head v)
 
decideAce :: Int -> Int --determines whether ace should be 1 or 11
decideAce t --total
    |t >= 11 = t + 1
    |t < 11  = t + 11

acesCounter :: [Int] -> Int -> Int --counts 1s and 11s in a list
acesCounter v w --list of card vals, aces counter
    |(take 1 v) == []                        = w
    |(take 1 v) == [1] || (take 1 v) == [11] = acesCounter (drop 1 v) (w + 1)
    |(take 1 v) /= [1] || (take 1 v) == [11] = acesCounter (drop 1 v) w

endgame :: String -> [PlayingCards] -> [PlayingCards] -> IO ()
endgame c disDHand disPHand = do
    putStrLn ("Dealer's Hand: " ++ show [(val x,su x) | x <- disDHand])
    putStrLn ("Your Hand: " ++ show [(val x,su x) | x <- disPHand])
    when (c == "1") $ do
        putStrLn "You busted!"
    when (c == "2") $ do
        putStrLn "You won!"
    when (c == "3") $ do
        putStrLn "The dealer won!"
    when (c == "4") $ do
        putStrLn "You tied!"
    putStrLn "Thank you for playing!"
