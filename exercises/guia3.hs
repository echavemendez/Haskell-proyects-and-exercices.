
--ambosSon0: dados dos numeros racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una usando pattern matching y otra no).
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b | a == 0 && b == 0 = True
              | otherwise = False


ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM a b = False

--mismoIntervalo: dados dos numeros reales, indica si est´an relacionados considerando la relaci´on de equivalencia en R cuyas clases de equivalencia son: (−∞, 3],(3, 7] y (7, ∞), o dicho de otra forma, si pertenecen al mismo intervalo.

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo a b | a > 3 && a <= 7 && b > 3 && b <= 7 = True
                   | a < 3 && b < 3 = True
                   | a > 7 && b > 7 = True
                   | otherwise = False

--esMultiploDe: dados dos n´umeros naturales, decidir si el primero es m´ultiplo del segundo.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | mod a b == 0 = True  
                 | otherwise = False


-- ej 3 "estanRelacionados" esto cumple todos los casos pedidos (que para todo a,b exista un k que a*a + a*b*k= 0), la razon por la que esto funciona es porque k es entero y unicamente -a*b*k = a*a es si b*k= a como b y k son enteros, solo pueden ser iguales si es una descomposicion de a, y ambos valores deben ser multiplos (como no puedo poner condiciones en k, pido que todos los b sean multiplos y el valor de k se "autorellena")

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | a /= 0 && b /= 0 && mod a (b) == 0 = True
                      | otherwise = False 


-- ej 4 
-- a) prodInt: calcula el producto interno entre dos tuplas R × R.
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (a,b) (c,d) = (a * c) + (b * d) 

--c) distanciaPuntos: calcula la distancia entre dos puntos de R2

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt(((a-c)^2) + ((b-d)^2))

--e) sumarSoloMultiplos: dada una terna de n´umeros enteros y un natural, calcula la suma de los elementos de la terna queson m´ultiplos del n´umero natural

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a, b, c) d | mod a d == 0 && mod b d == 0 && mod c d == 0 = a + b + c
                               | mod a d == 0 && mod b d == 0 = a + b
                               | mod a d == 0 && mod c d == 0 = a + c
                               | mod b d == 0 && mod c d == 0 = b + c
                               | mod a d == 0 = a
                               | mod b d == 0 = b
                               | mod c d == 0 = c
                               | otherwise = 0
                               
 -- ej 5 implementar la funcion todosMenores :: (Integer, Integer, Integer) ->Bool . Para este problema necesito a la funcion f y a la funcion g

funcionF :: Int -> Int 
funcionF n | n <= 7 = (n * n)
           | n > 7 = (2*n) -1


funcionG :: Int -> Int
funcionG n | mod n 2 == 0 = n `div` 2
           | otherwise = (3*n) + 1



todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a, b, c) | funcionF(a) > funcionG(a) && funcionF(b) > funcionG(b) && funcionF(c) > funcionG(c) = True
                       | otherwise = False

--ej 6 Programar una funci´on bisiesto :: Integer ->Bool s

bisiesto :: Int -> Bool
bisiesto x | mod x 4 /= 0 = False 
           | mod x 100 == 0 && mod x 400 /= 0 = False
           | otherwise = True


--7. Implementar una funci´on:distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (a, b, c) (d, e, f) | ((a-d) + (b-e) + (c-f)) < 0 = -((a-d) + (b-e) + (c-f))
                                       | ((a-d) + (b-e) + (c-f)) >= 0 = ((a-d) + (b-e) + (c-f))


--8. Implementar una funci´on comparar :: Integer ->Integer ->Integer  requiere otra funcion
sumaUltimosDosDigitos :: Int -> Int 
sumaUltimosDosDigitos a = (mod a 10) + (mod (a `div`10) 10 )


comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos (b) = 1 
             | sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b)  = -1
             | sumaUltimosDosDigitos(a) == sumaUltimosDosDigitos(b)  = 0


