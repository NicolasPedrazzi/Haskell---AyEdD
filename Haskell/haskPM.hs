


--En esta sección se utilizará el método de "Pattern Matching" y la lectura operacional de Haskell para la resoución de problemas.
--PRÁCTICO 1:


data Forma = Circulo | Cuadrado | Triangulo | Rombo 
     deriving (Show, Eq)
data Color = Rojo | Amarillo | Azul | Verde
     deriving (Show, Eq)

type Figura = (Forma, Color, Int)

--Laboratorio 1:

--⟨Pi : 0 ≤ i < #xs : tam.(xs.i) ⟩
sumaTam :: [Figura] -> Int
sumaTam [] = 0
sumaTam ((f, c, t):xs) = t + sumaTam xs

--⟨Qi : 0 ≤ i < #xs : tam.(xs.i) ⟩
prodTam :: [Figura] -> Int
prodTam [] = 1
prodTam ((f, c, t):xs) = t * prodTam xs

--⟨N i : 0 ≤ i < #xs : rombo.(xs.i) ⟩
contarRombo :: [Figura] -> Int
contarRombo [] = 0
contarRombo ((Rombo, c, t):xs) = 1 + contarRombo xs
contarRombo ((f, c, t):xs) = contarRombo xs

--⟨N i : 0 ≤ i < #xs : rombo.(xs.i) ∧ rojo.(xs.i) ⟩
contarRomboRojo :: [Figura] -> Int
contarRomboRojo [] = 0
contarRomboRojo ((Rombo, Rojo, t):xs) = 1 + contarRomboRojo xs
contarRomboRojo ((f, c, t):xs) = contarRomboRojo xs

--Laboratorio 2:

--La suma de los tamaños de todas las figuras de xs es mayor a 10.
sumTam10 :: [Figura] -> Bool
sumTam10 [] = True
sumTam10 [(a,b,c)] = c > 10
sumTam10 (x:xs) = sumaTam xs > 10

--Ninguna figura de xs tiene tamaño menor a 7.
tamMen7 :: [Figura] -> Bool
tamMen7 [] = True
tamMen7 ((a, b, c):xs) = c > 7 && tamMen7 xs

--Laboratorio 3: 

sumLista :: [Int] -> Int
sumLista [] = 0
sumLista (x:xs) = x + sumLista xs

--Laboratorio 4: 

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)

---Laboratorio 6:

--Producto de los elementos pares de xs.
--Función auxiliar de "prodPares":
listaPar :: [Int] -> [Int]
listaPar [] = []
listaPar (x:xs) | (mod x 2 == 0) = x : listaPar xs
                | otherwise = listaPar xs

prodPares :: [Int] -> Int
prodPares [] = 1
prodPares xs = productoria (listaPar xs)

sumPosPar :: [Int] -> Int
sumPosPar [] = 0
sumPosPar (x:y:xs) = y + sumPosPar xs

--Laboratorio 7:

data Carrera = Matematica | Fisica | Computacion | Astronomia
     deriving (Show, Eq)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

data NotaBasica = Si | La | Sol | Fa | Mi | Re | Do
     deriving (Show, Eq, Ord)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--LAboratorio 10:

type Altura = Int
type NumCamiseta = Int

data Zona = Arco | Defensa | Mediocampo | Delantera
    deriving (Show)
data TipoReves = DosManos | UnaMano
    deriving (Show)
data Modalidad = Carretera | Pista | Monte | BMX
    deriving (Show)
data PiernaHabil = Izquierda | Derecha
    deriving (Show) 

type ManoHabil = PiernaHabil

data Deportista = Ajedrecista
                | Ciclista Modalidad 
                | Velocista Altura 
                | Tenista TipoReves ManoHabil Altura  
                | Futbolista Zona NumCamiseta PiernaHabil Altura
    deriving (Show)

contarVelocistas :: [Deportista] -> Int
contarVelocistas [] = 0
contarVelocistas ((Velocista a):xs) = 1 + contarVelocistas xs
contarVelocistas (x:xs) = contarVelocistas xs

contarFutbolistas :: [Deportista] -> Zona -> Int
contarFutbolistas [] z = 0
contarFutbolistas ((Futbolista Arco n p a):xs) Arco = 1 + contarFutbolistas xs Arco
contarFutbolistas ((Futbolista Defensa n p a):xs) Defensa =  1 + contarFutbolistas xs Defensa
contarFutbolistas ((Futbolista Mediocampo n p a):xs) Mediocampo =  1 + contarFutbolistas xs Mediocampo
contarFutbolistas ((Futbolista Delantera n p a):xs) Delantera =  1 + contarFutbolistas xs Delantera
contarFutbolistas (x:xs) z = contarFutbolistas xs z 


--PRÁCTICO 2:


--sumCuad.xs = ⟨Pi : 0 ≤ i < #xs : xs.i ∗ xs.i⟩
sumCuad :: [Int] -> Int
sumCuad [] = 0
sumCuad (x:xs) = x*x + sumCuad xs

--iga.e.xs = ⟨ ∀ i : 0 ≤ i < #xs : xs.i = e ⟩
iga :: [Int] -> Int -> Bool
iga [] e = True
iga (x:xs) e = (x == e) && iga xs e

--exp.x.n = x^n
expo :: Int -> Int -> Int
expo x 0 = 1
expo x n = x * expo x (n-1)

--sumPar.n = ⟨Pi : 0 ≤ i ≤ n ∧ par.i : i⟩
--Función auxiliar "par.i"
par :: Int -> Bool
par i = mod i 2 == 0

sumPar :: Int -> Int
sumPar 0 = 0
sumPar n | (par n == True) = n + sumPar (n-1)
         | (par n == False) = sumPar (n-1)

--cuantos.p.xs = ⟨N i : 0 ≤ i < #xs : p.(xs.i)⟩
--Esta funcón es un gaso general de la anterior. Definiendo "p" como una función local.

--Laboratorio 6:

data Cola = Vacia | Encolada Deportista Cola
    deriving (Show)

--Ejemplo: (Encolada Ajedrecista (Encolada (Ciclista BMX) (Encolada (Velocista 170)) Vacia))

--"encolar :: Deportista -> Cola -> Cola, que agrega una persona a una cola de deportistas, en la última posición."
encolar :: Deportista -> Cola -> Cola
encolar dep Vacia = Encolada dep Vacia
encolar dep (Encolada d c) = Encolada d (encolar dep c)





























































