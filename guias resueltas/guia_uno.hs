--FUNCION DE USO
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x: xs) = f x xs (recr f z xs)

--EJERCICIO 1
{- max2 :: (Float, Float) -> Float 
-- normaVectorial :: (Float, Float) -> Float
-- subtract :: Float -> Float -> Float
-- predecesor ::  Float -> Float 
-- evaluarEnCero :: (t1 -> Float) -> Float
-- dosVeces :: (t1 -> Float) -> Float           ???
-- flipAll :: [a -> b -> c] -> [b -> a -> c]
-- flip :: (a -> b -> c) -> b -> a -> c
-- flipRaro :: flip ( (a -> b -> c) -> b -> a -> c) :: b -> (a -> b -> c) -> a -> c
-}
{-
    las no currificadas son las primeras dos.
    max2 :: Float -> Float -> Float 
    normaVectorial :: Float -> Float -> Float 
-}

--EJERCICIO 2 
--I
curryM :: ((a, b) -> c )-> a -> b -> c
curryM f x y = f (x, y)
--II
uncurryM :: (a -> b -> c) -> (a, b) -> c
uncurryM f (x, y) = f x y
--III
-- no se podra, ya que no hay forma de especificar una cantidad indefinida de parametros

--EJERCICIO 3
--I
sumF :: [Int] -> Int
sumF = foldr (+) 0

elemF :: Eq a => a -> [a] -> Bool
elemF n = foldr (\x rec -> (x == n) || rec ) False

masmas ::  [a] -> [a] -> [a]
masmas xs ys = foldr (:) ys xs
--esta funcion hace x:(foldr : ys xs)
--cuando termino xs (=[]), devuelvo ys que seria el caso base 
--tiene sentido porque el output es x1 : x2: x3: ys

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x rec -> if f x then x: rec else rec) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x rec -> f x : rec) []

--II
--foldruno f xs = if null xs then error "Lista vacia" else foldr (\x rec -> if null rec then x else f x rec)
mejorSegunF :: (a -> a -> Bool) -> [a] -> a
mejorSegunF f = foldr1 (\x rec -> if f x rec then x else rec)
--en foldr el caso base es la lista vacia[]
--en foldr1, el caso base es la lista con un solo elemento, y te devuelve ese elemento (foldr1 _ [x] = x)
--luego, son muy utiles para hacer comparaciones sobre listas

--III
sumaParciales :: Num a => [a] -> [a]
sumaParciales = reverse . foldl (\acc x -> (x + cabeza acc) : acc) []

cabeza :: Num a => [a] -> a
cabeza [] = 0
cabeza (x: xs)= x

--IV
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr1 (-)

--V
sumaInv :: Num a => [a] -> a
sumaInv xs = sumaAlt (reverse xs)

--EJERCICIO 4

partes :: [a] -> [[a]]
partes = foldr (\ x rec -> rec ++ map (x :) rec) [[]]

--TODO

--EJERCICIO 5
--La primera no es recursion estructural porque el caso recursivo utiliza el parametro xs fuera de g xs 
-- la segunda si es recursion estructural, luego la derfinimos con foldr

--Esta es la estructura original (mas entendible a la lectura)
entrelazarOG ::[a] -> [a] -> [a]
entrelazarOG  [] ys= ys
entrelazarOG (x: xs) ys = if null ys then x: entrelazarOG xs [] else x: head ys:  entrelazarOG xs (tail ys)

--Luego, intento pasarlo como lambda para poder usar foldr
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                          then x : entrelazar xs []
                          else x : head ys : entrelazar xs (tail ys)

--Por ultimo, lo implementamos con foldr
entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr = foldr (\ x rec -> \ys -> if null ys then x: rec [] else x: head ys: rec (tail ys)) id

--EJERCICIO 6
--a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if e == x then xs else x : rec) []

--b)
--En esta funcion necesito la posibilidad de devolver xs cuando encontre el elemento, y con foldr
--eso no es posibe, ya que la funcion que se le pasa como parametro solo acepta dos parametros.
--Si quisiera usar "rec" en el lambda de foldr para indicar que si e == x, saque el elemento y siga 
--con los demas casos, foldr sacara x (si e ==x) en todas las apariciones de la lista, por lo que no
-- tengo forma de "terminar antes".
--Formalmente, foldr se utiliza para funciones que utilizan esquemas de recursion estructural. En el caso
-- de sacarUna, es evidente que se utiliza recursion primitiva, por la utilizacion de xs fuera del caso recursivo.

--c)
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if x > e then e : x : xs else x : rec) [e]

--EJERCICIO 7
--I
genLista :: a -> (a->a) -> Integer -> [a]
genLista e f 0 = []
genLista e f n = f e : genLista (f e) f (n-1)

--II
desdeHasta :: (Integer, Integer) -> [Integer]
desdeHasta (x, y)= genLista (x-1) (\x -> x+1) (y - x)

--EJERCICIO 8

--I
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

--II 
armarPares1:: [a] -> [b] -> [(a, b)]
armarPares1= foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

--III
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares1 xs ys)

--EJERCICIO 9
--I
sumaMat :: [[Int]] ->[[Int]] ->[[Int]]
sumaMat = mapDoble (mapDoble (+))

--EJERCICIO 10 
--I
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

--II
potencia :: Integer -> Integer -> Integer
potencia n = foldNat (\_ rec -> n * rec) 1

--EJERCICIO 11
--del ejercicio
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

foldPoli :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b -> Polinomio a -> b
foldPoli fCte fSuma fProd cb pol = case pol of
                                    X -> cb
                                    Cte p -> fCte p
                                    Suma p q -> fSuma (rec p) (rec q)
                                    Prod p q -> fProd (rec p) (rec q)
                                    where rec = foldPoli fCte fSuma fProd cb

evaluarPoli :: Num a => a -> Polinomio a -> a
evaluarPoli = foldPoli id (+) (*)

--EJERCICIO 12
data AB a = Nil | Bin (AB a) a (AB a)
--I
foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB f cb ab = case ab of
                  Nil -> cb
                  Bin i r d -> f (foldAB f cb i) r (foldAB f cb d)

recAB :: (AB a -> a-> AB a -> b -> b -> b) -> b -> AB a -> b
recAB f cb ab = case ab of
                Nil -> cb
                Bin i r d -> f i r d (rec i) (rec d)
                where rec = recAB f cb

--II 
esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Integer
altura = foldAB (\reci raiz recd -> 1 + max reci recd) 0

cantNodos :: AB a -> Integer
cantNodos = foldAB (\reci r recd -> 1 + reci + recd) 0

--III
--TODO

--EJERCICIO 13 
ramasAB :: AB a -> [a]
ramasAB = foldAB (\ rei r red -> r: rei ++ r : red) []

cantHojas :: AB a -> Integer
cantHojas = foldAB (\rei r red -> if rei == 0 && red == 0 then 1 else rei + red) 0

espejo :: AB a -> AB a
espejo Nil = Nil
espejo (Bin i r d) = Bin d r i

--EJERCICIO 15 
--ROSETREES <3
--I
data RoseTree a = Rose a [RoseTree a]
-- Rose 1 [Rose 2 [Rose 3 [], Rose 4 [Rose 5 [], Rose 6[]]]]

--II
foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT fRose (Rose n hijos) = fRose n (map (foldRT fRose) hijos)
