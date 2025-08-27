{-# LANGUAGE KindSignatures #-}

module Main
  ( Queue(..)
  , SQueue
  , queue_of_nums
  , main
  ) where

import Data.List (intersperse, foldl')

class Queue (q :: * -> *) where
  emptyQueue :: q a
  isEmpty    :: q a -> Bool
  enqueue    :: a -> q a -> q a
  dequeue    :: q a -> (a, q a)

data SQueue a = SQ [a] [a]

check :: SQueue a -> SQueue a
check (SQ [] bs) = SQ (reverse bs) []
check q          = q

instance Queue SQueue where
  emptyQueue      = SQ [] []
  isEmpty (SQ [] []) = True
  isEmpty _          = False
  enqueue x (SQ fs bs) = check $ SQ fs (x:bs)
  dequeue (SQ [] [])   = error "can not dequeue on empty queue"
  dequeue (SQ (f:fs) bs)= (f, check (SQ fs bs))

instance (Eq a) => Eq (SQueue a) where
  q1 == q2 = toList q1 == toList q2
    where toList (SQ fs bs) = fs ++ reverse bs

instance (Show a) => Show (SQueue a) where
  show q = "q[" ++
           concat (intersperse "," (map show (toList q))) ++
           "]"
    where toList (SQ fs bs) = fs ++ reverse bs

instance Functor SQueue where
  fmap f (SQ fs bs) = SQ (map f fs) (map f bs)

queue_of_nums :: Queue q => Int -> Int -> q Int
queue_of_nums a b
  | a > b     = emptyQueue
  | otherwise = foldl' (flip enqueue) emptyQueue [a..b]

main :: IO ()
main = return ()
