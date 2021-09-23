-- Clase 4 de Funcional - Orden superior - 16/09/2021
module Library where
import PdePreludat

-- * Ejercicios
-- haciendo nuestra propia version de map
miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f (x:xs) = f x : miMap f xs

sumar1 x = x + 1
multiplicarPor2 x = 2 * x 
aplicarAlReves x f = f x

-- probar en ghci:
-- map sumar1 [1, 2, 3]
-- map (aplicarAlReves 4) [sumar1, multiplicarPor2] 
-- ^^ Que es lo que hace??

esVocal x = elem x "aeiouAEIOU"

-- haciendo nuestra propia version de filter
miFilter :: (a -> Bool) -> [a] -> [a]
miFilter cond [] = []
miFilter cond (x:xs)
    | cond x = x : miFilter cond xs
    | otherwise = miFilter cond xs

-- probar en ghci:
-- miFilter esVocal "hola"
-- miFilter esVocal ['o', 'l', 'a']

miFoldl f valorInicial [] = valorInicial
miFoldl f valorInicial (x:xs) = miFoldl f (f valorInicial x) xs 

sumar2Numeros x y = x + y

-- usar miFoldl

sumatoria numeros = miFoldl sumar2Numeros 0 numeros
-- sumatoria [1,2,3]
-- miFoldl sumar2Numeros 0 [1,2,3]
-- miFoldl sumar2Numeros (sumar2Numeros 0 1) [2,3]
-- miFoldl sumar2Numeros 1 [2,3]
-- miFoldl sumar2Numeros (sumar2Numeros 1 2) [3]
-- miFoldl sumar2Numeros 3 [3]
-- miFoldl sumar2Numeros (sumar2Numeros 3 3) []
-- miFoldl sumar2Numeros 6 []
-- 6

producto lista = miFoldl (*) 1 lista

-- a -> b -> a
-- cruzar :: Caballo -> Caballo -> Caballo
-- Ejemplo de foldl aplicado al trabajo practico.
cruzar = undefined 
cruzarTodas caballo listaDeCaballos = foldl cruzar caballo listaDeCaballos

-- scanl
-- Esta funcion es muy similar a foldl, pero en vez de devolver el acumulador,
-- devuelve una lista con todos los resultados parciales. Por ejemplo:
--  foldl (+) 0 [1, 2, 3] devuelve la sumatoria de [1, 2, 3] = 0 + 1 + 2 + 3 = 6
--  scanl (+) 0 [1, 2, 3] devuelve los resultados de las sumas parciales de [1, 2, 3] =
--  [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3] = [0, 1, 3, 6]
miScanl :: (b -> a -> b) -> b -> [a] -> [b]
miScanl f acum [] = [acum]
miScanl f acum (x:xs) = acum : miScanl f (f acum x) xs

-- all
-- Toma una funcion, p, de tipo (a -> Bool) y una lista, l, de tipo [a].
-- Devuelve True si para todos los x de l, p x es True. False en caso contrario.

-- usando recursion
miAllRecursivo :: (a -> Bool) -> [a] -> Bool
miAllRecursivo p [] = False
miAllRecursivo p (x:xs)
    | p x = True && miAnyRecursivo p xs
    | otherwise = False -- no necesito seguir recorriendo la lista

-- usando foldl
miAll :: (a -> Bool) -> [a] -> Bool
miAll p lista = foldl (\acumulador x -> (p x) && acumulador) True lista
                                     --- ^^^ evaluo el predicado en x y hago un AND con el acumulador
-- any
-- Toma una funcion, p, de tipo (a -> Bool) y una lista, l, de tipo [a].
-- Devuelve True si para algun x de l, p x es True. False en caso contrario.

-- usando recursion
miAnyRecursivo :: (a -> Bool) -> [a] -> Bool
miAnyRecursivo p [] = False
miAnyRecursivo p (x:xs)
    | p x = True -- no necesito seguir recorriendo la lista.
    | otherwise = False || miAnyRecursivo p xs

-- usando foldl
miAny :: (a -> Bool) -> [a] -> Bool
miAny p lista = foldl (\acumulador x -> (p x) || acumulador) False lista
                                     --- ^^^ evaluo el predicado en x y hago un OR con el acumulador