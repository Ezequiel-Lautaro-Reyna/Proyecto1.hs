{-# LANGUAGE NPlusKPatterns #-}
--Proyecto 1--
--Ejercicio 1.a--
esCero :: Int -> Bool
esCero x = x==0
{-
Ejemplos

*Main> esCero 3 
False
*Main> esCero 0
True
-}
--Ejercicio 1.b--
esPositivo :: Int -> Bool
esPositivo x = x>0  
{-
Ejemplos

*Main> esPositivo (-1)
False
*Main> esPositivo 1
True
-}
--Ejercicio 1.c--
esVocal :: Char -> Bool
esVocal x = (x=='a' || x=='e' || x=='i' || x=='o' || x=='u')
{-
Ejemplos

*Main> esVocal 'l'
False
*Main> esPositivo 'a'
True
-}
--Ejercicio 1.d--
valorAbsoluto :: Int -> Int
valorAbsoluto x | x>=0 = x
                | x<0 = -x
{-
Ejemplos

*Main> valorAbsoluto 1
1
*Main> valorAbsoluto (-4)
4
-}
--Ejercicio 2.a--

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x==True = True && paratodo xs
                | x/=True = False
{-
Ejemplos

*Main> paratodo []
True
*Main> paratodo [True,True,False]
False
*Main> paratodo [True,True,True]
True
-}
--Ejercicio 2.b--
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
{-
Ejemplos

*Main> sumatoria [1,3,5]
9
*Main> sumatoria [-2,4,7,10]
19
-}
--Ejercicio 2.c--
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
{-
Ejemplos

*Main> productoria [1,3,5]
15
*Main> productoria [-2,4,7]
-56
-}
--Ejercicio 2.d--
factorial :: Int -> Int
factorial x | x==1 || x==0 = 1
            | x>1 = x*(factorial(x-1))
            | x<0 = x*(factorial(x+1))
{-
Ejemplos

*Main> factorial 0
1
*Main> factorial 5
120
*Main> factorial 1
1
-}
--Ejercicio 2.e--
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = (div (sumatoria (x:xs)) (length (x:xs)))
{-
Ejemplos

*Main> promedio [2,3,5]
3
*Main> promedio [6,7,9]
7
-}
--Ejercicio 3--
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n==x = True
                   | n/=x = pertenece n xs
{-
Ejemplos

*Main> pertenece 3 [2,5,89]
False
*Main> pertenece 3 [2,5,89,3]
True
-}
--Ejercicio 4.a--
paratodo' :: [a] -> (a->Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

{-
Ejemplos

*Main> paratodo' [2,5,89] esCero
False
*Main> paratodo' [0,0,0] esCero
True
-}

--Ejercicio 4.b--
existe' :: [a] -> (a->Bool) -> Bool
existe' [] t =  False
existe' (x:xs) t = t x || existe' xs t

{-
Ejemplos

*Main> existe' [2,5,89] esCero
False
*Main> existe' [0,4,0] esPositivo
True
-}


--Ejercicio 4.c--
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria'(x:xs) t = t x + sumatoria' xs t

{-
Ejemplos

*Main> sumatoria' [2,5,-89] valorAbsoluto
96
*Main> sumatoria' [2,5,0] valorAbsoluto
7
-}

--Ejercicio 4.d--
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t

{-
Ejemplos

*Main> productoria' [2,5] factorial
240
*Main> productoria' [2,5,0] valorAbsoluto
0
-}

--Ejercicio 5--
paratodo'' :: [a] -> (a->Bool) -> Bool
paratodo'' (x:xs) t =  paratodo' (x:xs) t

{-
Ejemplos

*Main> paratodo'' [2,4,5] esPositivo
True
*Main> paratodo'' [0,0,0] esCero
True
-}

--Ejercicio 6.a--
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar
  where
    esPar x = (mod x 2) == 0

{-
Ejemplos

*Main> todosPares [2,4,5]
False
*Main> todosPares [0,0,0] 
True
-}

--Ejercicio 6.b--
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs esMultiplo
  where
    esMultiplo x = (mod x n) == 0

{-
Ejemplos

*Main> hayMultiplo 2 [2,4,5]
True
*Main> hayMultiplo 2 [0,0,0] 
False
-}


--Ejercicio 6.c--
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] cuadrado
    where
        cuadrado x = (x^2)

{-
Ejemplos

*Main> sumaCuadrados 2
1
*Main> sumaCuadrados 6
55
-}


--Ejercicio 6.d--
existeDivisor :: Int -> [Int] -> Bool
existeDivisor n xs = existe' xs divide
  where
    divide x = (mod n x) == 0

{-
Ejemplos

*Main> existeDivisor 2 [3,5,7,9]
False
*Main> existeDivisor 4 [2,5,7,9]
True
-}

--Ejercicio 6.e--
esPrimo :: Int -> Bool
esPrimo n = n > 1 && not (existeDivisor n [2..n-1])

{-
Ejemplos

*Main> esPrimo 2
True
*Main> esPrimo 4
False
-}

--Ejercicio 6.f--
factorial' :: Int -> Int
factorial' x = productoria' [1..x] valorAbsoluto

{-
Ejemplos

*Main> factorial' 2
2
*Main> factorial' 4
24
-}

--Ejercicio 6.g--
multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = productoria' xs primo
    where
        primo x | esPrimo x = x
                | esPrimo x/= True = 1

{-
Ejemplos

*Main> multiplicaPrimos [3,5,7,9]
105
*Main> multiplicaPrimos [2,12,3,8,10]
6
-}

--Ejercicio 6.h--
--- FUNCION AUXILIAR fib ---
fib :: Int -> Int
fib n | n <= 1 = n
      | otherwise = fib(n-1) + fib(n-2)


-- Para encontrar la solución a este problema voy a necesitar crear otra función auxiliar llamada listaFib :: Int -> [Int], que genere una lista de los primeros n elementos de la sucesión (en orden inverso y que incluye al 0)

listaFib :: Int -> [Int]
listaFib 0 = [0]
listaFib 1 = [1, 0]
listaFib 2 = [1, 1, 0]
listaFib n = fib n : listaFib (n-1)

-- esFib --
esFib :: Int -> Bool
esFib t = existe' (listaFib 20) (==t)

{-
Ejemplos

*Main> esFib 2
True
*Main> esFib 12
False
-}

--Ejecicio 6.i--
todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib

{-
Ejemplos

*Main> todosFib [1,3,5]
True
*Main> todosFib [1,3,5,7]
False
-}


--Ejercicio 7--
{-
¿Que hacen estas funciones?
La funcion Map toma 2 argumentos (uno de los cuales es una funcion) y aplica la funcion a cada elemento de una lista, lo cual resulta en una lista con la aplicacion de la funcion en el mismo orden.
La funcion filter toma 2 argumentos (uno de los cuales es un predicado) y el resultado es la lista con los elementeos que cumplen el predicado.

¿A que equivale la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1?

equivale a [2, -3, 7, 3, -7]


¿Y la expresion filter esPositivo [1, -4, 6, 2, -8]?

equivale a [1,6,2]

-}
--Ejercicio 8--
--RECURSIÓN--
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x*2):duplica xs
{-
Ejemplos

*Main> duplica [3,4,5]
[6,8,10]

*Main> duplica []
[]

-}
--Map--
-- map (*2) xs --
{-
Ejemplos

*Main> map (*2) [3,4,5]
[6,8,10]

*Main> map (*2) []
[]

-}

--Ejercicio 9--
--Recusrion--
sonPrimos :: [Int] -> [Int]
sonPrimos [] = []
sonPrimos (x:xs) | esPrimo x = x : sonPrimos xs
                | otherwise = sonPrimos xs
{-
Ejemplos

*Main> sonPrimos [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
[2,3,5,7,11,13]

*Main> sonPrimos [1,2,3]
[2,3]
-}
--Filter--
--filter esPrimo xs--
{-
Ejemplos

*Main> filter esPrimo [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
[2,3,5,7,11,13]

*Main> filter esPrimo [1,2,3]
[2,3]
-}
--Ejercicio 6.g mejorado--
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = productoria (sonPrimos xs)

{-
Ejemplos

*Main> multiplicaPrimos' [1,2,3,5,6]
30
*Main> multiplicapPrimos' [3,4,7]
21

-}

--Ejercicio 10.a--

primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | n == x = x : primIgualesA n xs
                      |otherwise = []

{-
Ejemplos

*Main> primIgualesA 'a' "aaadaa"
"aaa"
*Main> primIgualesA 3 [3,5,6,7,3,4,5,3,3,3]
[3]
-}

--Ejercicio 10.b--

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n [] = []
primIgualesA' n xs = takeWhile (== n) xs

{-
Ejemplos

*Main> primIgualesA' 'a' "aaadaa"
"aaa"
*Main> primIgualesA' 3 [3,5,6,7,3,4,5,3,3,3]
[3]
-}

--Ejercicio 11.a--
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs)  | x == y = x : primIguales (y:xs)
                      | otherwise = [x] 
{-
Ejemplos

*Main> primIguales [1,3,4,3]
[1]

*Main> primIguales [1,1,4,3]
[1,1]

-}
--Ejercicio 11.b--
primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualesA' (head xs) xs 

{-
Ejemplos

*Main> primIguales' [4,5,6,4]
[4]

*Main> primIguales' [3,3,3,4,5,6]
[3,3,3]]

-}