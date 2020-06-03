module ListSeq where

import Seq
import Par

instance Seq [] where
    
  emptyS = []

  --singletonS :: a -> [a]
  singletonS x = [x]

  -- lengthS l = length l
  lengthS [] = 0
  lengthS (x:xs) = 1 + length xs

  -- nthS       :: s a -> Int -> a
  -- nth xs n = xs !! n
  nthS [] _ = error "Lista vacia"
  nthS (x:xs) 0 = x
  nthS (x:xs) n = nthS xs (n - 1)

  -- tabulateS  :: (Int -> a) -> Int -> s a
  -- tabulateS f n --> [f(0),f(1), ... , f(n)]

  tabulateS f 0 = emptyS
  tabulateS f n = let
                  (x,xs) = f n ||| tabulateS f (n-1)
                  in x : xs

-- mapS       :: (a -> b) -> s a -> s 
  mapS = undefined
  -- filterS    :: (a -> Bool) -> s a -> s a
  filterS = undefined
  -- appendS    :: s a -> s a -> s a
  appendS = undefined
  -- takeS      :: s a -> Int -> s a
  takeS = undefined
  -- dropS      :: s a -> Int -> s a
  dropS = undefined
  -- showtS     :: s a -> TreeView a (s a)
  showtS = undefined
  -- showlS     :: s a -> ListView a (s a)
  showlS = undefined
  -- joinS      :: s (s a) -> s a
  joinS = undefined
  -- reduceS    :: (a -> a -> a) -> a -> s a -> a
  reduceS = undefined
  -- scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
  scanS = undefined
  -- fromList   :: [a] -> s 
  fromList = undefined

