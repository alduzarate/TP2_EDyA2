module ListSeq where

import Seq
import Par

instance Seq [] where
    
  emptyS = emptyS_
  singletonS = singletonS_
  lengthS = lengthS_
  nthS = nthS_
  tabulateS = tabulateS_
  mapS = mapS_
  filterS = filterS_
  appendS = appendS_
  takeS = takeS_
  dropS = dropS_
  showtS = showtS_
  showlS = showlS_
  joinS = joinS_
  reduceS = reduceS_
  scanS = scanS_
  fromList = fromList_

emptyS_ = []

singletonS_ x = [x]

-- lengthS l = length l
lengthS_ [] = 0
lengthS_ (x:xs) = 1 + length xs

-- nth xs n = xs !! n
nthS_ [] _ = error "Lista vacia"
nthS_ (x:xs) 0 = x
nthS_ (x:xs) n = nthS_ xs (n - 1)

-- tabulateS f n --> [f(0),f(1), ... , f(n)]
tabulateS_ f 0 = emptyS_
tabulateS_ f n = tabulateS_' f n 0
                where
                    tabulateS_' f 0 _ = emptyS_
                    tabulateS_' f n i =  let
                                            (x,xs) = f i ||| tabulateS_' f (n-1) (i+1)
                                        in x : xs

mapS_ f [] = []
mapS_ f (x:xs) = let 
                    (y, ys) = f x ||| mapS_ f xs
                in  y : ys

filterS_ p [] = []
filterS_ p (x:xs) =  let 
                        (y, ys) = p x ||| filterS_ p xs
                    in  if y then x : ys else ys

-- appendS xs ys = xs ++ ys
appendS_ xs [] = xs
appendS_ [] ys = ys
appendS_ (x:xs) ys = x : (appendS_ xs ys)

--takeS lista n = take n lista
takeS_ [] _ = []
takeS_ xs 0 = []
takeS_ l@(x:xs) n | len <= n     = l
                | otherwise  = x : (takeS_ xs (n - 1))
      where len = lengthS_ l

-- dropS_ = drop
dropS_ [] _ = []
dropS_ xs 0 = xs
dropS_ l@(x:xs) n | len <= n     = [] 
                  | otherwise  = (dropS_ xs (n - 1))
      where len = lengthS_ l

showtS_ [] = EMPTY
showtS_ [x] = ELT x
showtS_ xs =  let
                  (l,r) = (takeS_ xs (div len 2)) ||| (dropS_ xs (div len 2))
                in NODE l r
                where len = lengthS_ xs

showlS_ [] = NIL
showlS_ (x:xs) = CONS x xs

-- joinS_ = concat
joinS_ [] = []
joinS_ (x:xs) = appendS_ x (joinS_ xs)

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = []
contract _ [x] = [x]
contract f (x:y:xs) = let (z, zs) = f x y ||| contract f xs
                      in z:zs

reduceS_ _ e [] = e
reduceS_ f e [x] = f x e
reduceS_ f e xs = let ctr = contract f xs
                      ys = reduceS_ f e ctr
                  in id ys
--TODO: hacer que vaya obteniendo los elementos en una sola pasada
obtElemento :: (a -> a -> a) -> [a] -> [a] -> Int -> a
obtElemento f xs ys i | even i         = nthS_ ys (div i 2)
                      | otherwise      = f (nthS_ ys (div i 2)) (nthS_ xs (i - 1))

armarLista :: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
armarLista f xs ys i n | i < n         = (obtElemento f xs ys i) : armarLista f xs ys (i + 1) n
                      | otherwise     = []

expand :: (a -> a -> a) -> [a] -> ([a], a) -> ([a], a)
expand f xs (ys, z) = (armarLista f xs ys 0 n, z)
      where
        n = lengthS_ xs
                      
scanS_ _ e [] = ([], e)
scanS_ f e [x] = ([e], f x e)
scanS_ f e xs = let ctr = contract f xs
                    ys = scanS_ f e ctr
                  in expand f xs ys
              
fromList_ = id

