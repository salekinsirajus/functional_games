{- split deck into 2 hands
this function uses the PlayingCards type and deck -}
splitdeck :: [PlayingCards] -> ([PlayingCards] , [PlayingCards])
splitdeck []   = ([] , [])
splitdeck deck = splitAt ((length(deck) + 1) `div` 2) deck
