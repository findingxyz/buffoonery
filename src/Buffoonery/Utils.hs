module Buffoonery.Utils where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

toBoth :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
toBoth f (x1, y1) (x2, y2) = (f x1 y1, f x2 y2)

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> (a -> m a)
untilM p t =
   let recourse x = if (not . p) x then t x >>= recourse else return x
   in  recourse

while :: Monad m => (a -> Bool) -> (a -> m a) -> (a -> m a)
while p t =
   let recourse x = if p x then t x >>= recourse else return x
   in  recourse

doWhile :: Monad m => (a -> Bool) -> (a -> m a) -> (a -> m a)
doWhile p t =
   let recourse x = t x >>= \l -> if p l then recourse l else return l
   in  recourse

