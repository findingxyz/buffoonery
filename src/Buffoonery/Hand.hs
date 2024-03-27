module Buffoonery.Hand where

import Data.List ( sortBy )
import Data.Ord ( comparing, Down(Down) )

import Buffoonery.Card ( Card )
import Buffoonery.Utils ( mapBoth, toBoth )

type Played = [Card]

data Hand = HighCard Card
          | Pair [Card]
          | TwoPair [Card]
          | ThreeKind [Card]
          | Straight [Card]
          | Flush [Card]
          | FullHouse ([Card], [Card])
          | FourKind [Card]
          | StraightFlush [Card]
          | FiveKind [Card]
          | FlushHouse [Card]
          | FlushFive [Card]
          deriving (Show, Eq, Ord)

identifyHand :: Played -> [Hand]
identifyHand hand = sortBy (comparing Down) undefined
  where _len = length hand

scoreHand :: [Hand] -> (Int, Int)
scoreHand [] = (0, 0)
scoreHand (hand:_) =
    let levels = 1 in
    case hand of
        HighCard _ -> toBoth (+) (5, 1) $ mapBoth (* levels) (1, 10)
        Pair _ -> (10, 2)
        TwoPair _ -> (20, 2)
        ThreeKind _ -> (30, 3)
        Straight _ -> (30, 4)
        Flush _ -> (35, 4)
        FullHouse _ -> (40, 4)
        FourKind _ -> (60, 7)
        StraightFlush _ -> (100, 8)
        FiveKind _ -> (100, 10)
        FlushHouse _ -> (120, 12)
        FlushFive _ -> (150, 14)
