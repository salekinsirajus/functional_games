{-
Victor Zuniga
Yuki Adams
Shaown Salekin
-}

import System.Random
import Control.Monad
import Control.Applicative
import Data.Time.Clock.POSIX
import System.IO.Unsafe

data Suit = Clubs|Diamonds|Hearts|Spades
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data PlayingCards = Card {val::Value, su::Suit}
    deriving (Show, Read)

    
allSuits = [(minBound::Suit) ..]
allValues = [(minBound::Value) ..]
buildDeck :: [Value] -> [Suit] -> [PlayingCards]
buildDeck a b = [Card x y | x <- a, y <- b]
deck = buildDeck allValues allSuits

integralTime = round (unsafePerformIO getPOSIXTime) :: Int

getRandom :: Int -> Int
getRandom n = integralTime `mod` n


shuffle :: [a] -> [a]
shuffle xs 
    | length xs < 2 = xs
    | otherwise = do
        let n = getRandom (length xs -1)
        [xs !! n] ++ shuffle (take n xs ++ drop (n+1) xs)
        
--split deck into 2 hands
splitdeck :: [PlayingCards] -> ([PlayingCards] , [PlayingCards])
splitdeck []   = ([] , [])
splitdeck deck = splitAt ((length(deck) + 1) `div` 2) deck

--assign decks to player 1, player 2 respectively

hand1 = fst(splitdeck(shuffle deck))
hand2 = snd(splitdeck(shuffle deck))

main = do
    putStrLn "Enter 1 for autoWar and 2 for intWar: "
    choice <- getLine
    when (choice /= "1" && choice /= "2") $ do
        main
    when (choice == "1") $ do
        autoWar hand1 hand2
    when (choice == "2") $ do
        intWar hand1 hand2

autoWar :: [PlayingCards] -> [PlayingCards] -> IO () 
autoWar hand1 hand2 = do
    let p1 = head(hand1)
    let p2 = head(hand2)
    when (length hand1 /= 0 && length hand2 /= 0) $ do
        when (val(p1) > val(p2)) $ do
            let winhand = (drop 1 (hand1 ++ [p1,p2])) 
            let losehand = (drop 1 hand2)
            let hand1 = winhand
            let hand2 = losehand
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "player 1 wins the round"
            autoWar hand1 hand2
        
        when (val(p1) < val(p2)) $ do 
            let winhand = (drop 1 (hand2 ++ [p1,p2]))
            let losehand = (drop 1 hand1)
            let hand2 = winhand
            let hand1 = losehand
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "player 2 wins the round"
            autoWar hand1 hand2
        
        when (val(p1) == val(p2)) $ do --tie
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "WAR"
            let tieWinner = warTie 5 hand1 hand2 --call recursive function and assign it to tieWinner
            let hand1 = fst(tieWinner)
            let hand2 = snd(tieWinner)
        
            autoWar hand1 hand2
    when (length hand1 == 0) $ do
        putStrLn "Player 2 wins War!"
        
    when (length hand2 == 0) $ do
        putStrLn "Player 1 wins War!"

intWar :: [PlayingCards] -> [PlayingCards] -> IO () 
intWar hand1 hand2 = do
    let p1 = head(hand1)
    let p2 = head(hand2)
    when (length hand1 /= 0 && length hand2 /= 0) $ do
        when (val(p1) > val(p2)) $ do
            let winhand = (drop 1 (hand1 ++ [p1,p2])) 
            let losehand = (drop 1 hand2)
            let hand1 = winhand
            let hand2 = losehand
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "player 1 wins the round"
            putStrLn "Press enter to start next round."
            x <- getLine
            intWar hand1 hand2
            putStrLn ""
        
        when (val(p1) < val(p2)) $ do 
            let winhand = (drop 1 (hand2 ++ [p1,p2]))
            let losehand = (drop 1 hand1)
            let hand2 = winhand
            let hand1 = losehand
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "player 2 wins the round"
            putStrLn "Press enter to start next round."
            x <- getLine
            intWar hand1 hand2
            putStrLn ""
        
        when (val(p1) == val(p2)) $ do --tie
            putStrLn ("p1 card: " ++ (show (val p1)))
            putStrLn ("p2 card: " ++ (show (val p2)))
            putStrLn "WAR"
            
            let tieWinner = warTie 5 hand1 hand2 --call recursive function and assign it to tieWinner
            when ((length (fst(tieWinner))) > (length hand1)) $ do
                putStrLn "Player 1 wins the round!"
            when ((length (snd(tieWinner))) > (length hand2)) $ do
                putStrLn "Player 2 wins the round!"
            let hand1 = fst(tieWinner)
            let hand2 = snd(tieWinner)
            putStrLn "Press enter to start next round."
            x <- getLine
            intWar hand1 hand2
            putStrLn ""
        putStrLn ""
    when (length hand1 == 0) $ do
        putStrLn "Player 2 wins War!"
        
    when (length hand2 == 0) $ do
        putStrLn "Player 1 wins War!"
    putStrLn ""


warTie :: Int -> [PlayingCards] -> [PlayingCards] -> ([PlayingCards],[PlayingCards])
warTie numba hand1 hand2 = 
    warPile numba (take numba hand1) (take numba hand2) hand1 hand2
    
warPile :: Int -> [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> ([PlayingCards],[PlayingCards])
warPile numba p1warpile p2warpile hand1 hand2 = 
    warCard numba (last p1warpile) (last p2warpile) hand1 hand2
    
warCard :: Int -> PlayingCards -> PlayingCards -> [PlayingCards] -> [PlayingCards] -> ([PlayingCards],[PlayingCards])
warCard numba p1war p2war hand1 hand2 
    | val(p1war) > val(p2war)  = p1Winner numba hand1 hand2
    | val(p1war) < val(p2war)  = p2Winner numba hand1 hand2
    | val(p1war) == val(p2war) = warTie (numba + 4) hand1 hand2
    
p1Winner :: Int -> [PlayingCards] -> [PlayingCards] -> ([PlayingCards],[PlayingCards])
p1Winner numba hand1 hand2 = ((drop numba hand1) ++ (take numba hand1) ++ (take numba hand2),(drop numba hand2))

p2Winner :: Int -> [PlayingCards] -> [PlayingCards] -> ([PlayingCards],[PlayingCards])
p2Winner numba hand1 hand2 = ((drop numba hand1),(drop numba hand2) ++ (take numba hand2) ++ (take numba hand1))
