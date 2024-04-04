module Buffoonery.Card where

import Data.List (delete)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Show)

data Suit = Diamonds | Clubs | Hearts | Spades
    deriving (Eq, Ord, Enum, Show)

data Edition = Normal | Foil | Holographic | Polychrome | Negative
    deriving (Eq, Ord, Show)

data Enhancement = Bonus | Glass | Gold | Lucky | Mult | Steel | Stone | Wild
    deriving (Eq, Ord, Show)

data Seal = GoldSeal | RedSeal | BlueSeal | PurpleSeal
    deriving (Eq, Ord, Show)

data Card = Card {
    rank :: Rank
  , suit :: Suit
  , edition :: Edition
  , enhancement :: Maybe Enhancement
  , seal :: Maybe Seal
} deriving (Eq, Ord, Show)

type Deck = [Card]
type Cards = [Card]

createStandardCard :: Rank -> Suit -> Card
createStandardCard r s = Card r s Normal Nothing Nothing

csc :: Rank -> Suit -> Card
csc = createStandardCard

standard52 :: Deck
standard52 = [ csc r s | r <- [Two ..], s <- [Diamonds ..] ]

chips :: Card -> Int
chips (Card Jack _ _ _ _) = 10
chips (Card Queen _ _ _ _) = 10
chips (Card King _ _ _ _) = 10
chips (Card Ace _ _ _ _) = 11
chips (Card r _ _ _ _) = fromEnum r + 1

addCard :: Card -> Deck -> Deck
addCard = (:)

removeCard :: Card -> Deck -> Deck
removeCard = delete

filterDeck :: (Card -> Bool) -> Deck -> Deck
filterDeck = filter

miniShow :: Card -> String
miniShow (Card r s ed en sl) =
    show r ++ " " ++ show s ++ " " ++ 
    show ed ++ dontshownothing en ++ dontshownothing sl

dontshownothing :: Show a => Maybe a -> String
dontshownothing Nothing = ""
dontshownothing (Just x) = show x ++ " "

minierShow :: Card -> String
minierShow (Card r s ed en sl) =
    showRank r ++ [head (show s)] ++
    dontshownormal ed ++ dontshownothing en ++ dontshownothing sl
    where showRank Ace = "A"
          showRank King = "K"
          showRank Queen = "Q"
          showRank Jack = "J"
          showRank r' = show $ fromEnum r' + 1
          dontshownormal Normal = ""
          dontshownormal e = show e ++ " "

