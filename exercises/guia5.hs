--Ejercicio 1. Definir las siguientes funciones sobre listas:

longitud ::  [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--ultimo :: [t] -> t seg´un la siguiente especificaci´on:


ultimo :: [t] -> t 
ultimo [t] = t
ultimo (x:xs) = ultimo xs


--principio :: [t] -> [t] seg´un la siguiente especificaci´on:


principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs

--reverso :: [t] -> [t] seg´un la siguiente especificaci´on:

reverso :: [t] -> [t]
reverso [] = []
reverso xs = ultimo xs : reverso (principio xs) 

--Ejercicio 2. pertenece :: (Eq t) => t -> [t] -> Bool seg´un la siguiente especificaci´on:


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs

--todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero s´ı y solamente s´ı todos sus ele-mentos son iguales

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:xs)| x /= head xs = False   
                   | otherwise = todosIguales xs


--todosDistintos :: (Eq t) => [t] -> Bool seg´un la siguiente especificaci´o

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos (x:xs) |xs == [] = True 
                      |x == head xs = False    
                      | otherwise = todosDistintos xs 



--hayRepetidos :: (Eq t) => [t] -> Bool seg´un la siguiente especificaci´on

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos (x:xs)| pertenece x xs == True = True    
                   | xs == [] = False
                   | otherwise = hayRepetidos xs



--quitar :: (Eq t) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina la primera aparici´on de x en la lista xs (de haberla).

quitar :: (Eq t) => t -> [t] -> [t]
quitar t (x:xs) | pertenece t (x:xs) == False = xs
                | t == x = xs
                | otherwise = x: quitar t xs


--quitarTodos :: (Eq t ) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina todas las apariciones de x en la lista xs (de haberlas)


quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos t (x:xs)| t == x && xs == [] = []
                    | pertenece t (x:xs) == False = (x:xs)
                    | t == x = quitarTodos t xs
                    | otherwise = x: quitarTodos t xs

--7. eliminarRepetidos :: (Eq t) => [t] -> [t] que deja en la lista una ´unica aparici´on de cada elemento, eliminando las repeticiones adicionales.


eliminarRepetidos :: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x: eliminarRepetidos(quitarTodos x xs)
                                


