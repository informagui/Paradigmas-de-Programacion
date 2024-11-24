{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
--EJERCICIO 1
{- Todas estas funciones trabajan con listas como parametros 
null: de tipo nulo (no tiene nada)
head: devuelve el primer elemento de una lista
tail: devuelve la lista sin el primer elemento
init: devuelve una lista sin el ultimo elemento
last: devuelve el ultimo elemento de una lista 
take: dado un entero n, devuelve la lista con los primero n elementos 
drop: dado un entero n, devuelva la lista sin los primeros n elementos
(++): concatena dos listas 
concat: concatena una lista de listas en una sola lista
reverse: devuelve una lista con el orden invertido
elem: dado un elemnto, retorna si este pertenece o no a una lista
-}

--EJERCICIO 2
valorAbsoluto :: Float -> Float
valorAbsoluto x | x < 0 = -x
                | otherwise = x

bisiesto :: Int -> Bool
bisiesto x | (mod x 4 /= 0) || ((mod x 100 == 0) && (mod x 400 /=0)) = False
            |otherwise = True

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x-1)

cantDivisores :: Int -> Int -> Int
cantDivisores x 1 = 1
cantDivisores x y | mod x y == 0 = 1 + cantDivisores x (y-1)

esPrimo :: Int -> Bool
esPrimo x   | cantDivisores x x == 2  = True
            |otherwise = False

cantDivPrim :: Int -> Int -> Int
cantDivPrim x 1 = 0
cantDivPrim x y | (mod x y == 0) && esPrimo y= 1 + cantDivisores x (y-1)

cantDivisoresPrimo :: Int -> Int
cantDivisoresPrimo x = cantDivPrim x x

--EJERCICIO 3
inverso :: Float -> Maybe Float
inverso n   | n/= 0 = Just(1 / n)
            |otherwise = Nothing

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right y) | y = 1
                | otherwise = 0


--EJERCICIO 4 
pertenece :: String -> Char -> Bool
pertenece [b] a = a == b
pertenece (x:xs) a  | x == a = True
                    | otherwise = pertenece xs a

limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x: xs) ys | pertenece ys x = limpiar xs (borrarCh ys x)
                        | otherwise = limpiar xs ys

borrarCh :: String -> Char -> String
borrarCh [] a = []
borrarCh (x: xs) a | x == a = xs
                    | otherwise = x : borrarCh xs a

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio xs) xs

promedio :: [Float] -> Float
promedio xs =  suma xs / largo xs

suma :: [Float] -> Float
suma [] = 0
suma (x: xs) = x + suma xs

largo :: [a] -> Float
largo [] = 0
largo (x: xs) = 1 + largo xs

todosIguales :: [Int] -> Bool
todosIguales [x] = True
todosIguales (x: y: xs) = (x == y) && todosIguales (y: xs)

--EJERCICIO 5
data AB a = Nil | Bin (AB a) a (AB a)

vacio :: AB a -> Bool
vacio Nil = True
vacio _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB (Bin Nil a Nil) = Bin Nil (not a) Nil
negacionAB (Bin x y z) = Bin (negacionAB x) (not y) (negacionAB z)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin x y z) = y * productoAB x * productoAB z