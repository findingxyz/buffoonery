module Buffoonery where

import Data.List (nub)

import Numeric.Probability.Simulation ((~.))
import qualified Numeric.Probability.Random as Rnd

import System.Random (Random)

import Buffoonery.Prob ( drawN, drawUntil )
import Buffoonery.Card ( Card(Card, suit), Suit(Spades) )

{-
expected (fmap (\([(Card _ s1 _ _ _), (Card _ s2 _ _ _), (Card _ s3 _ _ _)], _) -> if [s1, s2, s3] == [Spades, Spades, Spades] then 1 else 0) a)
expected $ fmap (length . fst) (_ :: Dist.T prob ([Card], [Card]))
-}

is3Spades :: ([Card], a) -> Bool
is3Spades ([Card _ s1 _ _ _, Card _ s2 _ _ _, Card _ s3 _ _ _], _) = [s1, s2, s3] == [Spades, Spades, Spades]
is3Spades _ = False

draw5 :: (Fractional prob, Ord prob, Random prob) => Rnd.Distribution prob ([Card], [Card])
draw5 = (10000 ~. const (drawN 5)) undefined

draw3 :: (Fractional prob, Ord prob, Random prob) => Rnd.Distribution prob ([Card], [Card])
draw3 = (10000 ~. const (drawN 3)) undefined

foursuits :: (Fractional prob, Ord prob, Random prob) => Rnd.Distribution prob ([Card], [Card])
foursuits = (10000 ~. const (drawUntil ((== 4) . length . nub . map suit))) undefined

