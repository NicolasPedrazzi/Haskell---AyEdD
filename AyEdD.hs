

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

data Color = Rojo | Amarillo | Verde | Azul   
                                           deriving (Show, Eq)
data Forma = Triangulo | Cuadrado | Rombo | Circulo 
                                           deriving (Show, Eq)

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

triangulosRojos :: [Figura] -> Bool
triangulosRojos [] = True
triangulosRojos ((a, b, c):xs) | ((a == Triangulo) && (b == Verde)) = False
                     | ((a == Triangulo) && (b == Amarillo)) = False
                     | ((a == Triangulo) && (b == Azul)) = False
                     | otherwise = True 


existeCuadVerde :: [Figura] -> Bool
existeCuadVerde [] = False
existeCuadVerde ((a, b, c):xs) = ((a == Cuadrado) && (b == Verde)) || existeCuadVerde xs

--Todos los circulos son azules y de tamaño menor a 10:
--Funcion auxiliar de tCAm10
cAm10 :: Figura -> Bool
cAm10 (a, b, c) = (a == Circulo) && (b == Azul) && (c < 10)

tCAm10 :: [Figura] -> Bool
tCAm10 [] = True
tCAm10 ((Circulo, b, c):xs) = cAm10 (Circulo, b, c) && tCAm10 xs
tCAm10 ((a, b, c):xs) = tCAm10 xs

--Ningún triángulo de xs es azul:
--Función auxiliar de nTAL
nTA :: Figura -> Bool
nTA (a, b, c) = (a == Triangulo) && (b /= Azul) 

nTAL :: [Figura] -> Bool
nTAL [] = True
nTAL ((Triangulo ,b ,c):xs) = nTA (Triangulo, b ,c) && nTAL xs
nTAL ((a, b, c):xs) = nTAL xs

--En xs no hay circulos amarillos ni verdes:
--Función auxiliar de nCAVL
nCAV :: Figura -> Bool
nCAV (a, b, c) = (a == Circulo) && (b /= Amarillo && b /= Verde)

nCAVL :: [Figura] -> Bool
nCAVL [] = True
nCAVL ((Circulo, b, c):xs) = nCAV (Circulo, b, c) && nCAVL xs
nCAVL ((a, b, c):xs) = nCAVL xs

--Existe (al menos) un cuadrado de tamaño menor a 5 en xs:
eCm5 :: [Figura] -> Bool
eCm5 [] = False
eCm5 ((Cuadrado, b, c):xs) = (c < 5) || eCm5 xs
eCm5 ((a, b, c):xs) = eCm5 xs

--Si hay circulos rojos en xs entonces hay cuadrados rojos
--Funciones auxiliares de hCReCR
hCiR :: Figura -> Bool
hCiR (a, b, c) = (a == Circulo) && (b == Rojo)
hCuR :: Figura -> Bool
hCuR (a, b, c) = (a == Cuadrado) && (b == Rojo)
    --Preguntar en clase (al probar con listas que conotienen un circulo Rojo y no hay un Cuadrado Rojo devuelve "True" y vicebersa)
    --Es un problema con los datos que ingrese el usuario?
    --Debo inculir un constructor en la funcion para que se agregue un cuadrado o un circulo en caso de no haberlo?
hCReCR :: [Figura] -> Bool
hCReCR [] = True
hCReCR ((Circulo, b , c):xs) = hCiR (Circulo, b, c) && hCReCR xs
hCReCR ((Cuadrado, b, c):xs) = hCuR (Cuadrado, b ,c) && hCReCR xs
hCReCR ((a, b, c):xs) = hCReCR xs








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














