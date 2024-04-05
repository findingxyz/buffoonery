module Buffoonery where

import Numeric.Probability.Simulation ((~.))
import qualified Numeric.Probability.Random as Rnd
import qualified Numeric.Probability.Distribution as Dist

import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad (when)

import Data.List (nub, sort)

import Control.Exception (catch, ErrorCall)

import System.Random (Random)

import Text.Read (readMaybe)

import Buffoonery.Prob ( drawN, drawUntil, expected, stdDev )
import Buffoonery.Card ( standard52, Card(..), Suit(..), Rank(..), Edition(..), csc, minierShow )
import Buffoonery.Hand ( Hand(..), countRanks, countSuits )

{-
expected (fmap (\([(Card _ s1 _ _ _), (Card _ s2 _ _ _), (Card _ s3 _ _ _)], _) -> if [s1, s2, s3] == [Spades, Spades, Spades] then 1 else 0) a)
expected $ fmap (length . fst) (_ :: Dist.T prob ([Card], [Card]))
-}

is3Spades :: ([Card], a) -> Bool
is3Spades ([Card _ s1 _ _ _, Card _ s2 _ _ _, Card _ s3 _ _ _], _) = [s1, s2, s3] == [Spades, Spades, Spades]
is3Spades _ = False

draw5 :: (Fractional prob, Ord prob, Random prob) => [Card] -> Rnd.Distribution prob ([Card], [Card])
draw5 deck = (10000 ~. const (drawN 5 deck)) undefined

draw3 :: (Fractional prob, Ord prob, Random prob) => [Card] -> Rnd.Distribution prob ([Card], [Card])
draw3 deck = (10000 ~. const (drawN 3 deck)) undefined

--foursuits :: (Fractional prob, Ord prob, Random prob) => [Card] -> Rnd.Distribution prob ([Card], [Card])
--foursuits deck = (10000 ~. const (drawUntil ((== 4) . length . nub . map suit) deck)) undefined

checkHand :: Hand -> [Card] -> Bool
checkHand _ [] = False
checkHand ph h =
    case ph of
        HighCard -> h /= [] || True
        Pair -> maximum (countRanks h) == 2
        TwoPair -> filter (== 2) (countRanks h) == [2, 2]
        ThreeKind -> maximum (countRanks h) == 3
        Straight -> isStraight h
        Flush -> isFlush h
        FullHouse -> isFullHouse h
        FourKind -> maximum (countRanks h) == 4
        StraightFlush -> isStraight h && isFlush h
        FiveKind -> maximum (countRanks h) == 5
        FlushHouse -> isFullHouse h && isFlush h
        FlushFive -> maximum (countRanks h) == 5 && isFlush h
        _ -> False
  where isFlush = any (>= 5) . countSuits
        isStraight h = longestStraight 1 h >= 5
        isFullHouse h = let counted = countRanks h in elem 2 counted && elem 3 counted

randomHand :: [Card]
randomHand = [ csc Ace Spades, csc Two Clubs, csc Ten Diamonds, csc Seven Hearts, csc Three Clubs, csc Four Clubs, csc Queen Spades ]

fstOf3 (x, _, _) = x

longestStraight :: Int -> [Card] -> Int
longestStraight o h = fstOf3 $ foldr (\r (len, prevmax, prev) ->
        undefined) (0, 0, head sorted) $ tail sorted
    where sorted = nub . sort $ map (fromEnum . rank) h

g = sort . map (fromEnum . rank)

--command :: (Fractional prob, Ord prob, Random prob) => String -> StateT [Card] (Dist.T prob) (Either String ([Card], [Card]))
command :: StateT ([Card], [Card]) IO Bool
command = do
    liftIO $ putStr "> "
    c <- liftIO getLine
    case words c of
        "create" : _ -> put ([], standard52) >> pure True
        "draw" : snum : _ ->
            case readMaybe snum of
                Nothing -> liftIO $ putStrLn "second argument invalid" >> pure True
                Just num -> do
                    (hand, deck) <- get
                    let lhand = length hand
                    if num > length deck
                    then liftIO $ putStrLn ("tried to draw too much (" ++ show num ++ " from " ++ show lhand ++ ")")
                    else do
                        (drawn, left) <- liftIO . Rnd.run $ drawN num deck
                        liftIO $ print drawn
                        put (hand ++ drawn, left)
                    pure True
        "drawInto" : phand : _ ->
            case readMaybe phand of
                Nothing ->
                    if phand == "All"
                    then undefined
                    else liftIO $ putStrLn "unknown hand type" >> pure True
                Just rhand -> do
                    (hand, deck) <- get
                    let sims = Rnd.run $ ((10000 ~. const (drawUntil (checkHand rhand) (hand, deck))) undefined :: Rnd.Distribution Float ([Card], [Card]))
                    tried <- liftIO $ catch (do
                        sims' <- liftIO sims
                        let draws = fmap (subtract (length hand) . length . fst) sims'
                        liftIO (putStr "mean, std: ")
                        --liftIO . print $ (expected draws, stdDev draws)) ((\e -> print e) :: ErrorCall -> IO ())
                        liftIO . print $ (expected draws, stdDev draws)) ((\_ -> putStrLn "impossible") :: ErrorCall -> IO ())
                    pure True
        "hand" : _ -> do
            (hand, _) <- get
            liftIO (putStrLn (show (length hand) ++ " card(s) in hand: ") >> print hand) >> pure True
        "deck" : _ -> do
            (_, deck) <- get
            liftIO (putStrLn (show (length deck) ++ " card(s) in hand: ") >> print (map minierShow deck)) >> pure True
        "exit" : _ -> pure False
        [] -> liftIO $ putStrLn "no command" >> pure True
        _ -> liftIO $ putStrLn "unknown command" >> pure True

buffoonery :: IO ()
buffoonery = evalStateT loop ([], [])
  where
    loop = do
        continue <- command
        when continue loop
