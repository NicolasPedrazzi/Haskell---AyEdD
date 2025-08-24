
--Práctico 1 Algoritmos y Estructuras de Datos I
--Laboratorio 1:


esCero :: Int -> Bool
esCero x = x == 0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x 
                | x < 0 = -x

 
--Laboratorio 2:


paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = ( x == True ) && paratodo xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = mod (length xs) (sumatoria xs)


--Laboratorio 3:


--Variables libres: xs
todoPositivo :: [Int] -> Bool                          --⟨ ∀ i : 0 ≤ i < #xs : xs.i > 0 ⟩
todoPositivo [] = True
todoPositivo (x:xs) = x > 0  && todoPositivo xs

existeCinco :: [Int] -> Bool                           --⟨ ∃ i : 0 ≤ i < #xs : xs.i = x ⟩
existeCinco [] = False
existeCinco (x:xs) = x == 5 || existeCinco xs

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs  --⟨ ∀ i : 0 ≤ i < #xs - 1 : xs.i = xs.(i + 1) ⟩


--Laboratorio 4:


productoriaBis :: Int -> Int
productoriaBis 0 = 1
productoriaBis x = x * ( productoriaBis (x - 1)) 

promedioBis :: [Int] -> Int
promedioBis [] = 0
promedioBis (x:xs) = mod (length xs) (sumatoria xs)

maxMinList :: [Int] -> [Int] -> Bool
maxMinList _ [] = error "Falta pirmera Lista"
maxMinList [] _ = error "Falta segunda Lista"
maxMinList (xs) (ys) = maximum xs < minimum ys

productoIgual :: Int ->Int -> Int -> Bool             --⟨ ∃ i, j : (2 ≤ i < n) ∧ (2 ≤ j < n) : i ∗ j = n ⟩
productoIgual x y z = x >= 2 && y >= 2 && x*y == z 


--Laboratorio 5:


todoTrue :: [Bool] -> Bool                          --todos.xs ≡ ⟨ ∀ i : 0 ≤ i < #xs : xs.i ⟩
todoTrue [] = True
todoTrue (x:xs) = x == True && todoTrue xs


--Labortorio 6: 


potencia2 :: Int -> Bool                            --"n es potencia de 2"
potencia2 x =  mod x 2 == 0

masGrandeN :: ([Int], Int) -> Bool                   --"n es el elemento más grande de xs"
masGrandeN ([], _) = False
masGrandeN (xs, x) = x == maximum xs

productoPar :: [Int] -> Int                         --"Producto de los elementos pares de xs"
productoPar [] = 1
productoPar (x:xs) | mod x 2 == 0 = x * productoPar xs
                   | otherwise = productoPar xs

posPar :: [Int] -> [Int]                            --"Suma de los elementos en posicion par"
posPar [] = []
posPar (x:xs) = x : posPar (drop 1 xs)

sumPosPar :: [Int] -> Int
sumPosPar [] = 0
sumPosPar (x:xs) = sum (posPar xs)


--Laboratorio 7:


paraTodo :: [a] -> (a -> Bool) -> Bool
paraTodo [] _ = True
paraTodo (x:xs) p = p x && paraTodo xs p

existe :: [a] -> (a -> Bool)  -> Bool
existe [] _ = False
existe (x:xs) p = p x || paraTodo xs p 

sumaToria :: [a] -> (a -> Int) -> Int
sumaToria [] _ = 0
sumaToria (x:xs) p = p x + sumaToria xs p 

producToria :: [a] -> (a -> Int) -> Int
producToria [] _ = 0
producToria (x:xs) p = p x + sumaToria xs p


--Laboratorio 9:


todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x:xs) = mod x 2 == 0 && todosPares xs

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo _ [] = False
hayMultiplo m (x:xs) = mod m x == 0 || hayMultiplo m xs

sumaCuadrados :: Int -> Int
sumaCuadrados n = sum [i^2 | i <- [0..n-1]]

existeDivisor :: Int -> [Int] -> Bool
existeDivisor _ [] = False
existeDivisor n (x:xs) = mod x n == 0 || existeDivisor n xs 

esPrimo :: Int -> Bool
esPrimo x = x > 1 && not (or [mod x y == 0 | y <- [2..floor (sqrt (fromIntegral x))], y /= x])

factorialBis :: Int -> Int
factorialBis x = product [1..x]

productoPrimos :: [Int] -> Int
productoPrimos [] = 0
productoPrimos (x:xs) = product (filter esPrimo xs)

fib :: Int -> Int  --Funcion auxiliar
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

esFib :: Int -> Bool
esFib n = any (== n) [fib x | x <- [0..]]

todosFib :: [Int] -> Bool
todosFib xs = and (map esFib xs)


--Laboratorio 10:


valorDoble :: Int -> Int
valorDoble x = 2*x

listaDoble :: [Int] -> [Int]
listaDoble [] = []
listaDoble (xs) = map valorDoble xs


--Práctico 2 Algoritmos y Estructuras de Datos I

--Laboratorio 2: Tipos Enumerados / Clases de Tipos

data Carrera = Matemática | Física | Computación | Astronomía deriving Eq

titulo :: Carrera -> String
titulo Matemática = "Licenciatura en Matematica"
titulo Física = "Licenciatura en Fisica"
titulo Astronomía = "Licenciatura en Astronomia"
titulo Computación = "Licenciatura en ciencias de la Computacion"

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Show)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do  = 'C'
cifradoAmericano Re  = 'D'
cifradoAmericano Mi  = 'E'
cifradoAmericano Fa  = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La  = 'A'
cifradoAmericano Si  = 'B'

--Laboratorio 4: Polimorfismo ad hoc

minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

data NotaGrave = DO | RE | MI | FA | SOL | LA | SI deriving (Eq, Ord, Bounded)

notaMasGrave :: [NotaGrave] -> NotaGrave
notaMasGrave [] = maxBound
notaMasGrave [x] = x 
notaMasGrave (x:xs) = min x (notaMasGrave xs)

--Laboratorio 5: Sinónimos de Tipos / Constructores con Parámetros

--Sinónimos de Tipos:
type Altura = Int
type NumCamiseta = Int
--Tipos Algebráicos sin Parámetros:
data Zona = Arco | Defensa | Mediocampo | Delantera deriving Eq
data Modalidad = Carretera | Pista | Monte 
data PiernaHábil = Izquierda | Derecha
data TipoRevés = DosManos | UnaMano
--Sinónimo:
type ManoHábil = PiernaHábil
--Deportista es un tipo Algebráico con constructones Paramétricos:
data Deportista = Ajedrecista                                         -- Constructor sin Argumentos
                 | Ciclista Modalidad                                  -- Constructor con un Argumento
                 | Velocista Altura                                    -- Constructor con un Argumento
                 | Tenista TipoRevés ManoHábil Altura                  -- Constructor con tres Argumentos
                 | Futbolista Zona NumCamiseta PiernaHábil Altura      -- constructor con cuatro Argumentos
type Arco = Zona

contarVelocistas :: [Deportista] -> Int
contarVelocistas [] = 0
contarVelocistas (x:xs) = 
                 case x of
                    Velocista _ -> 1 + contarVelocistas xs
                    _ -> contarVelocistas xs

contarFutbolistas :: [Deportista] -> Zona -> Int
contarFutbolistas [] _ = 0
contarFutbolistas (x:xs) zona =
                  case x of
                    Futbolista z _ _ _ -> if z == zona then 1 + contarFutbolistas xs z 
                    else contarFutbolistas xs zona
                    _ -> contarFutbolistas xs zona

--Laboratorio 10: Tipos Recursivos / clases de Tipos

sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

data Alteracion = Bemol | Sostenido | Natural    deriving Show
data NotaMusical = Nota NotaBasica Alteracion    deriving Show

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nb alteracion) =
  let baseSound = sonidoNatural nb
   in case alteracion of
          Sostenido -> baseSound + 1
          Bemol      -> baseSound - 1
          Natural    -> baseSound 

instance Eq NotaMusical where
  (==) :: NotaMusical -> NotaMusical -> Bool
  (==) nm1 nm2 = sonidoCromatico nm1 == sonidoCromatico nm2

instance Ord NotaMusical where
  (<=) :: NotaMusical -> NotaMusical -> Bool
  nm1 <= nm2 = sonidoCromatico nm1 <= sonidoCromatico nm2

--Laboratorio 11: Tipos enumerados con Polimorfismo

primerElemento :: [ a ] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

--Laboratorio 12: Tipos Recursivos 
















