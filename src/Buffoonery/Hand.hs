module Buffoonery.Hand where

import Data.List ( sortBy )
import Data.Ord ( comparing, Down(Down) )

import Buffoonery.Card ( Card )
import Buffoonery.Utils ( mapBoth, toBoth )

type Played = [Card]

data Hand = HighCard
          | Pair
          | TwoPair
          | ThreeKind
          | Straight
          | Flush
          | FullHouse
          | FourKind
          | StraightFlush
          | FiveKind
          | FlushHouse
          | FlushFive
          deriving (Show, Read, Eq, Ord)

data PlayedHand = PHighCard Card
                | PPair [Card]
                | PTwoPair [Card]
                | PThreeKind [Card]
                | PStraight [Card]
                | PFlush [Card]
                | PFullHouse ([Card], [Card])
                | PFourKind [Card]
                | PStraightFlush [Card]
                | PFiveKind [Card]
                | PFlushHouse [Card]
                | PFlushFive [Card]
                deriving (Show, Eq, Ord)

identifyHand :: Played -> [Hand]
identifyHand hand = sortBy (comparing Down) undefined
  where _len = length hand

scoreHand :: [PlayedHand] -> (Int, Int)
scoreHand [] = (0, 0)
scoreHand (hand:_) =
    let levels = 1 in
    case hand of
        PHighCard _ -> toBoth (+) (5, 1) $ mapBoth (* levels) (1, 10)
        PPair _ -> (10, 2)
        PTwoPair _ -> (20, 2)
        PThreeKind _ -> (30, 3)
        PStraight _ -> (30, 4)
        PFlush _ -> (35, 4)
        PFullHouse _ -> (40, 4)
        PFourKind _ -> (60, 7)
        PStraightFlush _ -> (100, 8)
        PFiveKind _ -> (100, 10)
        PFlushHouse _ -> (120, 12)
        PFlushFive _ -> (150, 14)
