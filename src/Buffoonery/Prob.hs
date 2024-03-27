module Buffoonery.Prob where

import Numeric.Probability.Percentage (Dist)
import qualified Numeric.Probability.Random as Rnd
import qualified Numeric.Probability.Distribution as Dist

import Control.Monad.Trans.State (StateT(StateT, runStateT), evalStateT)

import qualified Data.List.HT as ListHT

import Control.Monad (replicateM)

import System.Random (Random)


import Buffoonery.Card
    ( Cards,
      Card(..),
      standard52 )
import Buffoonery.Utils ( untilM, while, doWhile )

selectOne :: (Fractional prob) => StateT [a] (Dist.T prob) a
selectOne = StateT $ Dist.uniform . ListHT.removeEach

select :: (Fractional prob) => Int -> [a] -> Dist.T prob [a]
select n = evalStateT (replicateM n selectOne)

draw :: (Fractional prob) => ([Card], Cards) -> Dist.T prob ([Card], Cards)
draw (cards,cl) = runStateT (fmap (:cards) selectOne) cl

drawF :: ([Card], Cards) -> Dist ([Card], Cards)
drawF = draw

drawN :: Int -> Rnd.T ([Card], [Card])
drawN n =
   doWhile
      (\(cards,_) -> length cards < n)
      (Rnd.change drawF) ([], standard52)

drawWhile :: ([Card] -> Bool) -> Rnd.T ([Card], [Card])
drawWhile p =
   while
      (\(cards, _) -> p cards) (Rnd.change drawF) ([], standard52)

drawUntil :: ([Card] -> Bool) -> Rnd.T ([Card], [Card])
drawUntil p =
   untilM
      (\(cards, _) -> p cards) (Rnd.change drawF) ([], standard52)

expected :: (Fractional prob, Integral a) => Dist.T prob a -> prob
expected = sum . fmap (\(x, p) -> fromIntegral x * p) . Dist.decons

simulated :: (Fractional prob, Ord prob, Random prob) => (a -> Bool) -> Rnd.Distribution prob a -> IO prob
simulated p rd = do
    ds <- Rnd.run rd
    pure $ expected (fmap (\r -> if p r then (1 :: Int) else 0) ds)

chanceP :: (Fractional prob, Ord prob, Random prob) => (a -> Bool) -> Rnd.Distribution prob a -> Rnd.T (Dist.T prob Bool)
chanceP p = fmap (Dist.map p)
