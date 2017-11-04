import System.Random
import Control.Monad

getRandom :: Int -> Int
getRandom n = 1234435 `mod` n

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
deck = buildDeck allValues allSuits

greaterCard :: PlayingCards -> PlayingCards -> Char
greaterCard a b
    |(val a) > (val b) = '>'
    |(val a) < (val b) = '<'
    |otherwise = '='

--Games

main = do
    putStrLn "What game should be played?" --here the player chooses which game they would like to play
    putStrLn "Enter 1 for Magic 8 Ball"
    putStrLn "Enter 2 for AutoWar"
    putStrLn "Enter 3 for InteractiveWar"
    putStrLn "Enter 4 for Blackjack"
    putStrLn "Enter q to exit"

    choice <- getLine
    when (choice /= "q") $ do
        when (choice == "4") $ do
            blackjack deck

--Blackjack

blackjack :: [PlayingCards] -> IO () --This function sets up the dealer's and player's hands, then calls hitOrStay to complete the game
blackjack d = do
    let dHand = []
    let disDHand = dHand ++ (take 2 d)
    let dTemp = drop 2 d
    let d = dTemp
    let hidDHand = head disDHand
    putStrLn ("Dealer's Hand: " ++ show hidDHand ++ " ****")
    let pHand = []
    let disPHand = pHand ++ (take 2 d)
    let dTemp = drop 2 d
    let d = dTemp
    putStrLn ("Your Hand: " ++ show disPHand)
    hitOrStay d disDHand disPHand
    putStrLn "Thank you for playing!"

hitOrStay :: [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> IO () --This function handles the player's hit or stay choices
hitOrStay d disDHand disPHand = do
        putStr "Would you like to hit or stay? "
        choice <- getLine
        when (choice /= "hit" && choice /= "stay") $ do
            putStrLn "Invalid - Please enther 'hit' or 'stay'"
            hitOrStay d disDHand disPHand
        when (choice == "hit") $ do
            let pHTemp = disPHand ++ (take 1 d)
            let dTemp = drop 1 d
            let disPHand = pHTemp
            let d = dTemp
            let bust = calculate disPHand
            when (bust == False) $ do
                hitOrStay d disDHand disPHand
            when (bust == True) $ do
                putStrLn "end"
                --endgame "1"
        when (choice == "stay") $ do
            putStrLn "dhit"
            --dHitOrStay d disDHand
        putStrLn " bl"

calculate :: [PlayingCards] -> Bool --calculate whether the player has busted
calculate d
    |total d <= 21 = False
    |total d > 21  = True

total :: [PlayingCards] -> Int --calculate the total of cards in a hand
total d = 0--helper3 [helper2 x | x <- d]

helper2 :: PlayingCards -> Int --convert card value to a number value
helper2 c
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

helper3 :: [Int] -> [Int] --move aces to end of hand (recursion doesn't work perfectly yet)
helper3 v
    |((null (tail v)) /= True) && (head v == 1) = helper3 ((tail v) ++ [head v])
    |((null (tail v)) /= True) && (head v /= 1) = helper3 (tail v)
    |((null (tail v)) == True)                  = v

helper4 :: [Int] -> Int -> Int --calculate whether an ace should be 11 or 1 (incomplete)
helper4 v t
    |((null (tail v)) /= True) && (head v /= 1) = helper4 (tail v) (t + head v)
    |((null (tail v)) /= True) && (head v == 1) = helper4 (tail v) (helper5(t + head v))
    |((null (tail v)) == True)                  = 0
 
helper5 :: Int -> Int --
helper5 v = 0

{-endgame :: String -> IO ()
endgame c = do
    when (c == "1")
        putStrLn "You busted!"
    when (c == "2")
        putStrLn "You won!"
    when (c == "3")
        putStrLn "The dealer won!"
    putStrLn "bl"-}
    --dhos something = do
        --putStrLn "blah"

    --calculate winner

    --putStrLn "Thank you for playing!"


{- --pseudocode
game
    dealPlayer
    hit or stay IO
    dealDealer
    hit or stay auto
    revea
dealPlayer :: [PlayingCards] ~~
dealPlayer 
    give player 2 random cards



dealCard

-}
