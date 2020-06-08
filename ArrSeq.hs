module ArrSeq where

import Seq
import Par
import qualified Arr as A
import  Arr ((!))

instance Seq A.Arr where
  emptyS = A.empty
  singletonS = singletonS_
  lengthS = A.length
  nthS = (!)
  tabulateS = A.tabulate
  mapS = mapS_
  filterS = filterS_
  appendS = appendS_
  takeS = takeS_
  dropS = dropS_
  showtS = showtS_
  showlS = showlS_
  joinS = A.flatten
  reduceS = undefined
  scanS = undefined
  fromList = A.fromList


singletonS_ x = fromList [x]

mapS_ f xs = tabulateS (\i -> (f (nthS xs i))) (lengthS xs)

filterS_ p xs | len == 0     = emptyS
              | len == 1     = if p (nthS xs 0) then xs else emptyS
              | otherwise    = let 
                                  m = (div len 2)
                                  (ys, zs) = filterS_ p (takeS_ xs m) ||| filterS_ p (dropS_ xs m)
                               in appendS_ ys zs
    where len = lengthS xs

appendS_ xs ys = tabulateS f (n + m)
    where 
      n = lengthS xs
      m = lengthS ys
      f i | i < n      = nthS xs i
          | otherwise  = nthS ys (i - n)

takeS_ xs n | len < n         = xs
            | otherwise       = A.subArray 0 n xs
    where len = lengthS xs

dropS_ xs n | len < n         = emptyS
            | otherwise       = A.subArray n (len - n) xs
    where len = lengthS xs

showtS_ xs | len == 0       = EMPTY
           | len == 1       = ELT (nthS xs 0)
           | otherwise      = let
                                 m = (div len 2)
                                 (l,r) = (takeS_ xs m) ||| (dropS_ xs m)
                              in NODE l r
    where len = lengthS xs

-- Consulta  -> showlS_ (singletonS x) = CONS x NIL ¿por qué no se puede?
-- showlS_ emptyS = NIL --> matchea todo con esto
-- al implementar las funciones, deberíamos usar los nombres de las funciones de la clase o de nuestra
-- implementación especial para esta instancia de Seq?
showlS_ xs | len == 0       = NIL
           | otherwise      = CONS (nthS xs 0)  xq
    where len = lengthS xs
