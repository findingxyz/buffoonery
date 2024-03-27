module Main where

import Buffoonery

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let card = Card Ace Hearts Normal Nothing Nothing
  print card
