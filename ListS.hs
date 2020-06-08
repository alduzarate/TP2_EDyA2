module ListS where

-- TODO: Preguntar append = ++ (Ejemplo)

data TreeView a t = EMPTY | ELT a | NODE t t
                    deriving Show
data ListView a t = NIL | CONS a t
                    deriving Show

(|||)   ::   a -> b -> (a,b)
a ||| b = (a,b)

emptyS :: [a]
emptyS = []

singletonS :: a -> [a]
singletonS x = [x]

lengthS :: [a] -> Int
-- lengthS l = length l
lengthS [] = 0
lengthS (x:xs) = 1 + length xs

-- nth xs n = xs !! n
nthS :: [a] -> Int -> a
nthS [] _ = error "Lista vacia"
nthS (x:xs) 0 = x
nthS (x:xs) n = nthS xs (n - 1)

-- tabulateS f n --> [f(0),f(1), ... , f(n)]
tabulateS :: (Int -> b) -> Int -> [b]
tabulateS f 0 = emptyS
tabulateS f n = tabulateS' f n 0
                where
                    tabulateS' f 0 _ = emptyS
                    tabulateS' f n i =  let
                                            (x,xs) = f i ||| tabulateS' f (n-1) (i+1)
                                        in x : xs

mapS :: (a -> b) -> [a] -> [b]
mapS f [] = []
mapS f (x:xs) = let 
                    (y, ys) = f x ||| mapS f xs
                in  y : ys

filterS :: (a -> Bool) -> [a] -> [a]
filterS p [] = []
filterS p (x:xs) =  let 
                        (y, ys) = p x ||| filterS p xs
                    in  if y then x : ys else ys

appendS    :: [a] -> [a] -> [a]
-- appendS xs ys = xs ++ ys
appendS xs [] = xs
appendS [] ys = ys
appendS (x:xs) ys = x : (appendS xs ys)

takeS:: [a] -> Int -> [a]
--takeS lista n = take n lista
takeS [] _ = []
takeS xs 0 = []
takeS l@(x:xs) n | len <= n     = l
                 | otherwise  = x : (takeS xs (n - 1))
      where len = lengthS l

dropS :: [a] -> Int -> [a]
-- dropS = drop
dropS [] _ = []
dropS xs 0 = xs
dropS l@(x:xs) n | len <= n     = [] 
                     | otherwise  = (dropS xs (n - 1))
      where len = lengthS l

showtS :: [a] -> TreeView a [a]
showtS [] = EMPTY
showtS [x] = ELT x
showtS xs =  let
                    (l,r) = (takeS xs (div len 2)) ||| (dropS xs (div len 2))
                in NODE l r
                where len = lengthS xs

showlS :: [a] -> ListView a [a]
showlS [] = NIL
showlS (x:xs) = CONS x xs

joinS :: [[a]] -> [a]
-- joinS = concat
joinS [] = []
joinS (x:xs) = appendS x (joinS xs)

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = []
contract _ [x] = [x]
contract f (x:y:xs) = let (z, zs) = f x y ||| contract f xs
                      in z:zs

-- contract (-) 0 [1,2,3] = 
--   (-1,zs) = -1 ||| contract (-) [3] = (-1,[3]) --> devuelvo -1:[3] = [-1,3]

contract (-) [3] = [3]

reduceS :: (a -> a -> a) -> a -> [a] -> a
reduceS _ e [] = e
reduceS f e [x] = f x e
reduceS f e xs = let ctr = contract f xs
                     ys = reduceS f e ctr
                 in id ys

obtElemento :: (a -> a -> a) -> [a] -> [a] -> Int -> a
obtElemento f xs ys i | even i         = nthS ys (div i 2)
                      | otherwise      = f (nthS ys (div i 2)) (nthS xs (i - 1))

armarLista :: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
armarLista f xs ys i n | i < n         = (obtElemento f xs ys i) : armarLista f xs ys (i + 1) n
                       | otherwise     = []

expand :: (a -> a -> a) -> [a] -> ([a], a) -> ([a], a)
expand f xs (ys, z) = (armarLista f xs ys 0 n, z)
      where
        n = lengthS xs                     

expand (-) [-1,3] ([0],4) = 




scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanS _ e [] = ([], e)
scanS f e [x] = ([e], f e x)
scanS f e xs = let ctr = contract f xs
                   ys = scanS f e ctr
               in expand f xs ys

fromList   :: [a] -> [a] 
fromList = id

-- scanS (-) 0 [1,2,3] = 
--   ctr = contract (-) 0 [1,2,3] = [-1,3]
--   yr = scanS (-) 0 [-1,3] --> devuelvo expand (-) [1,2,3] scanS (-) 0 [-1,3]

-- scanS (-) 0 [-1,3] = 
--   ctr = [-4]
--   yr = scanS (-) 0 [-4] --> devuelvo expand (-) [-1,3] ([0],4])


-- scanS (-) 0 [-4] = ([0], 0 - -4) = ([0],4)
-- ------------------------------------------------------------------------------

-- scanS (-) 0 [1,2,3,4,5,6] = 
--   ctr = contract (-) 0 [1,2,3,4,5,6] = [-1,-1,-1]
--   yr = scanS (-) 0 [-1,-1,-1] --> devuelvo expand (-) [1,2,3,4,5,6] scanS (-) 0 [-1,-1,-1]
{-
contract (-) 0 [1,2,3,4,5,6] = 
  (-1,zs) = 1 - 2 ||| contract (-) [3,4,5,6] --> devuelvo [-1,-1,-1]

contract (-) 0 [3,4,5,6] = 
  (-1, zs) = 3 - 4 ||| contract (-) [5,6] --> devuelvo [-1,-1]

contract (0) 0 [5,6]=
  (-1,zs) = 5-6 ||| contract (-) [] --> devuelvo -1:contract (-) 0 [] = [-1]
-}

-- scanS (-) 0 [-1,-1,-1] = 
--   ctr = [0,-1]
--   yr = scanS (-) 0 [0,-1] --> devuelvo expand (-) [-1,-1,-1] scanS (-) 0 [0,-1]

-- scanS (-) 0 [0,-1] =
--   ctr = [1]
--   yr = scanS (-) 0 [1] --> devuelvo expand (-) [0,-1] scanS (-) 0 [1] = expand (-) [0,-1] ([0], 1 - 0)


-- scan ⊕ b hx0, x1, x2, x3, x4, x5i =
-- (hb,
-- b ⊕ x0,
-- b ⊕ (x0 ⊕ x1),
-- (b ⊕ (x0 ⊕ x1)) ⊕ x2,
-- b ⊕ ((x0 ⊕ x1) ⊕ (x2 ⊕ x3)),
-- (b ⊕ ((x0 ⊕ x1) ⊕ (x2 ⊕ x3))) ⊕ x4i,
-- b ⊕ (((x0 ⊕ x1) ⊕ (x2 ⊕ x3)) ⊕ (x4 ⊕ x5))
-- )
