module Buffoonery.Card where

data Rank = Unranked | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
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

standard52 :: Deck
standard52 = [ Card r s Normal Nothing Nothing | r <- [Two ..], s <- [Diamonds ..] ]

chips :: Card -> Int
chips (Card Unranked _ _ _ _) = 0
chips (Card Jack _ _ _ _) = 10
chips (Card Queen _ _ _ _) = 10
chips (Card King _ _ _ _) = 10
chips (Card Ace _ _ _ _) = 11
chips (Card r _ _ _ _) = fromEnum r + 1
