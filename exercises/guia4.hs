



--ej 2 Implementar una función parteEntera :: Float ->Integer 



-- ej 3 Especificar e implementar la funci´on esDivisible :: Integer ->Integer ->Bool que dados dos n´umeros naturales determinar si el primero es divisible por el segundo. No est´a permitido utilizar las funciones mod ni div.

restaDivisible :: Int -> Int -> Int
restaDivisible a b | (a-b) == 0 = 1
                   | (a-b) < 0 = 0  
                   | otherwise = restaDivisible (a-b) b


esDivisible :: Int ->Int ->Bool
esDivisible x y = restaDivisible x y == 1

-- es a-b == 0? es True q es divisible, es a-b < 0? no es divisible =False, es a-b > 0? agarra a a-b y restale b


modulo::Int->Int->Int
modulo a b | a>=0 && a<b = a
           | otherwise = modulo (a-b) b


parteEntera::Float->Int
--parteEntera=truncate
parteEntera x | x<1 && x>=0 =0
              | x>(-1) && x<=0 = -1
              | x>=1 = 1+parteEntera (x-1)
              | otherwise =(-1)+parteEntera (x+1)

--Especificar e implementar la funci´on sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros n n´umeros impares. Por ejemplo: sumaImpares 3 ❀ 1+3+5 ⇝ 9.

-- impar = 2k+1 quiero sumar todo numero que pueda ser expresado como 2k+1 que sea menor que n

sumaImpares :: Int -> Int 
sumaImpares a | a == 0 = 0 
              | otherwise = moduloImpar(a) + sumaImpares(a-1)

moduloImpar :: Int -> Int
moduloImpar a | mod a 2 == 0 = 0
              | mod a 2 /= 0 = a


--Ejercicio 5. Implementar la funci´on medioFact :: Integer ->Integer que dado n ∈ N calcula n!! = n (n−2)(n−4)· · · .

medioFact :: Int ->Int
medioFact a | a == 0 = 1
            | a == -1 = 1
            | otherwise = a * medioFact(a-2)

--Ejercicio 6. Especificar e implementar la funci´on sumaDigitos :: Integer ->Integer que calcula la suma de d´ıgitos de un n´umero natural. Para esta funci´on pueden utilizar div y mod.
sumaDigitos :: Int -> Int
sumaDigitos a | a == 0 = 0
              | mod a 10 == a = a
              | otherwise = mod a 10 + sumaDigitos(div a 10)

--Ejercicio 7. Implementar la funci´on todosDigitosIguales :: Integer ->Bool que determina si todos los d´ıgitos de un n´umero natural son iguales



sonTodosIguales :: Int -> Int
sonTodosIguales a | mod a 10 == a = 1
                  | mod (div a 10) 10 /= mod a 10 = 0
                  | otherwise = sonTodosIguales(div a 10)

todosDigitosIguales :: Int -> Bool
todosDigitosIguales a | sonTodosIguales(a) == 1 = True
                      | sonTodosIguales(a) == 0 = False

cantDigitos :: Int -> Int
cantDigitos n | mod n 10 == n = 1
              | mod n 10 /= n = 1 + cantDigitos(div n 10)
              


iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = mod (div n (10^(cantDigitos(n)-i))) 10


{-Ejercicio 9. Especificar e implementar una funci´on esCapicua :: Integer ->Bool que dado n ∈ N≥0 determina si n es
un n´umero capic´ua. -}

ultimoDigito :: Int -> Int 
ultimoDigito n = mod n 10

nesimoDigito :: Int -> Int
nesimoDigito n = mod (div n (10^(cantDigitos(n)-1))) 10


esCapicua :: Int -> Bool
esCapicua n = ultimoDigito(n) == nesimoDigito(n)



{-Ejercicio 10. Especificar, implementar y dar el tipo de las siguientes funciones (s´ımil Ejercicio 4 Pr´actica 2 de ´Algebra 1).
-}

f1 :: Int -> Int
f1 n | n == 0 = 1
     | otherwise = 2^(n) + f1(n-1)

f2 :: Int -> Float -> Float 
f2 n q | n == 1 = q
       | otherwise = q^(n) + (f2 (n-1) q)


f3 :: Int -> Float -> Float
f3 n q | n == 1 = q*2
       | otherwise = q^(2*n) + (f2 ((2*n)-1) q)


{-se puede pensar tambien como f3 n q = f2 2*n q -}


f4 :: Int -> Float -> Float
f4 n q =  f2 (2*n) q - f2 n q


{-- Ejercicio 11. a) Especificar e implementar una funci´on eAprox :: Integer ->Float que aproxime el valor del n´umero e
a partir de la siguiente sumatoria:
-}

factorial :: Int -> Int 
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)


eAprox :: Int -> Float 
eAprox n | n == 0 = 1
         | otherwise =  1/(fromIntegral (factorial n)) + (eAprox (n-1))



{-Ejercicio 12. Para n ∈ N se define la sucesi´on
-}

raizDe2Aprox :: Int ->Float
raizDe2Aprox n | n == 1 = 2
               | otherwise = (2 + 1/(raizDe2Aprox (n-1)) )


formalRaiz2 :: Int -> Float
formalRaiz2 n = (raizDe2Aprox n ) - 1

{- Ejercicio 13 doble sumatoria i^j
-}

sumatoriaInterna :: Int -> Int -> Int
sumatoriaInterna n m | m == 0 = 0
                     | otherwise = n^m + sumatoriaInterna n (m-1)

sumatoriaTotal :: Int -> Int -> Int
sumatoriaTotal n m | n == 0 = 0 
                   | otherwise = sumatoriaTotal (n-1) m + sumatoriaInterna n m

{-      Especificar e implementar una funci´on sumaPotencias :: Integer ->Integer ->Integer ->Integer que
dados tres naturales q, n, m sume todas las potencias de la forma q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m. Esto es basicamente una doble sumatoria con la base fija (q) y los exponentes m y n rotativos. -}


sumatoriaInternaM :: Int -> Int -> Int -> Int
sumatoriaInternaM q n m | m == 1 = q
                      | otherwise = q^(n+m) + sumatoriaInternaM q n (m-1)

sumatoriaExternaN :: Int -> Int -> Int -> Int
sumatoriaExternaN q n m | n == 1 = q
                      | otherwise = q^(n+m) + sumatoriaExternaN q (n-1) m  


sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q n m = (sumatoriaExternaN q n m) + (sumatoriaInternaM q n m)

--Especificar e implementar una funci´on sumaRacionales :: Integer ->Integer ->Float que dados dos naturales n, m sume todos los n´umeros racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m, 


sumaRacionales :: Int ->Int ->Float
sumaRacionales n m | n == 1 = iteracionDeM 1 m
                   | otherwise = iteracionDeM n m + sumaRacionales (n-1) m



iteracionDeM :: Int ->Int -> Float
iteracionDeM n m | m == 1 = fromIntegral n
                 | otherwise = (fromIntegral n / fromIntegral m) + iteracionDeM n (m-1)
        

--ej 1 guia 4

fibonacci :: Int -> Int
fibonacci n | n==1 = 1
            | n==0 = 0
            | otherwise =  fibonacci(n-1) + fibonacci(n-2)

--Ejercicio 17. Implementar la funci´on esFibonacci :: Integer ->Bool

esFibonacci :: Int ->Bool 
esFibonacci n | n == 0 = True   
              | 

puedeSerFibonacci :: Int -> Int
puedeSerFibonacci n | n == fibonacci n = 1
                    
                    | otherwise = puedeSerFibonacci