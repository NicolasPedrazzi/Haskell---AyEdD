
--Algorítmos y Estructuras de Datos I
--Práctico 0:

--Laboratorio 1:

esCero :: Int -> Bool
esCero x = x == 0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x
                | x < 0  = (-x) 

--Laboratorio 2:

todosPositivos :: [Int] -> Bool
todosPositivos [] = True
todosPositivos (x:xs) = (x > 0) && todosPositivos xs  

hayPares :: [Int] -> Bool
hayPares [] = True
hayPares (x:xs) = (mod x 2 == 0) && hayPares xs

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = (x == True) && paratodo xs

existe :: [Bool] -> Bool
existe [] = False
existe (x:xs) = (x == True) || existe xs

--Laboratorio 3:

todosPos :: [Int] -> Bool                      
todosPos [] = True                                    
todosPos (x:xs) = (x > 0) && (todosPos xs)  
                                                                                              
--existeIgual :: [Int] -> Int -> Bool                                                      --(Analizar ambas funciones)
--existeIgual [] n = False
--existeIgual (x:xs) n = (x == n) || (existeIgual xs)

--todosIguales :: [Int] -> Bool
--todosIguales [] = True
--todosIguales [x] = True
--todosIguales (x:xs) = (x == (head xs)) && (todosIguales xs) 

--LAboratorio 4:

data Color = Rojo | Amarillo | Verde | Azul   deriving (Show, Eq)
data Forma = Triangulo | Cuadrado | Rombo | Circulo deriving (Show, Eq)
type Figura = (Forma, Color, Int)

tam :: Figura -> Int
tam (a, b, c) = c

--Laboratorio 5:

todosRojos :: [Figura] -> Bool
todosRojos [] = True
todosRojos ((a, b, c):xs) = (b == Rojo) && todosRojos xs 

existeRombo :: [Figura] -> Bool
existeRombo [] = False
existeRombo ((a, b, c):xs) = (a == Rombo) || existeRombo xs

--Laboratorio 6:

todoAmarillo :: [Figura] -> Bool
todoAmarillo [] = True
todoAmarillo ((a, b, c): xs) = (b == Amarillo) && todoAmarillo xs

tamMayorSiete :: [Figura] -> Bool
tamMayorSiete [] = True
tamMayorSiete ((a, b, c):xs) = (c > 7) && tamMayorSiete xs

tamMenorCinco :: [Figura] -> Bool
tamMenorCinco [] = True
tamMenorCinco ((a, b, c):xs) = (c < 5) && tamMenorCinco xs


triangulosRojos :: [Figura] -> Bool                                                             --(Analizar ambas versiones)
triangulosRojos [] = True
triangulosRojos ((a, b, c):xs) = ((a == Triangulo) && (b == Rojo)) && triangulosRojos xs  

tRbis :: [Figura] -> Bool
tRbis [] = True
tRbis ((a, b, c):xs) | ((a == Triangulo) && (b == Verde)) = False
                     | ((a == Triangulo) && (b == Amarillo)) = False
                     | ((a == Triangulo) && (b == Azul)) = False
                     | otherwise = True 


existeCuadVerde :: [Figura] -> Bool
existeCuadVerde [] = False
existeCuadVerde ((a, b, c):xs) = ((a == Cuadrado) && (b == Verde)) || existeCuadVerde xs


existeCircAzulmenor10 :: [Figura] -> Bool                                           --(No cumple la especificación (Preguntar))
existeCircAzulmenor10 [] = False
existeCircAzulmenor10 ((a, b, c):xs) = ((a == Circulo) && (b == Azul) && (c < 10)) || existeCircAzulmenor10 xs


noTrianguloAzul :: [Figura] -> Bool                                                 --(No cumple la especificación (Preguntar))
noTrianguloAzul [] = True
noTrianguloAzul ((a, b, c):xs) = ((a == Triangulo) && (b /= Azul)) && noTrianguloAzul xs


--Práctico 1:

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + (sumaLista xs)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + (sumatoria xs)

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x*(productoria xs)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*(factorial (n-1))

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs) 














