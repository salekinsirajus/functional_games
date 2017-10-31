data Suit = Clubs|Diamonds|Hearts|Spades
            deriving (Show, Eq, Ord, Enum, Bounded)

data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
             deriving (Show, Eq, Ord, Enum, Bounded)

data PlayingCards = Card (Value,Suit)
                    deriving (Show, Eq)

allSuits = [(minBound::Suit) ..]
allValues = [(minBound::Value) ..]
buildDeck:: [Value] -> [Suit] -> [PlayingCards]
buildDeck a b = map Card [(x,y) | x <- a, y <- b]
deck = buildDeck allValues allSuits

greaterCard :: PlayingCards -> PlayingCards -> Int
greaterCard (Card (a,b)) (Card (c,d))
    |a > c = 1
    |a < c = 2
    |otherwise = 3
