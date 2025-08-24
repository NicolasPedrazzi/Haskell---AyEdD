--PRÁCTICO 1--

promedio :: Float -> Float -> Float
promedio x y = (x + y) / 2

multiplicar :: Int -> Int -> Int
multiplicar x y =x*y

f :: Int -> Int 
f x = 5*x

duplica :: Int -> Int
duplica x = x + x

por2 :: Int -> Int
por2 x = 2*x

--FUNCIONES DEFINIDDAS POR CASOS--

signo :: Int -> Int
signo x | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0
        
entre0y9 :: Int -> Bool
entre0y9 x | 0 < x && x < 9 = True
           | otherwise = False

rangoPrecio :: Int -> String 
rangoPrecio x | x < 2000 && x >0 = "Muy Barato"
              | x > 5000 = "Muy Caro"
              | x > 2000 && x < 5000 = "Hay que verlo"
              | x <= 0 = "Imposible"

absoluto :: Int -> Int 
absoluto x | x >= 0 = x
           | x < 0 = -x

esMultiplo2 :: Int -> Bool
esMultiplo2 x | mod x 2 == 0 = True
              | otherwise = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

esBiciesto :: Int -> Bool
esBiciesto x | (mod x 400 == 0) = True
             | (mod x 4 == 0)   = True
             | (mod x 100 == 0) = False
             | otherwise        = False

dispersion :: Int -> Int -> Int -> Int
dispersion a b c = maximum [a, b, c] - minimum [a, b, c]

celsiusToFhar :: Float -> Float
celsiusToFhar x = (x*1.8) + 32

fahrToCelsius :: Float -> Float
fahrToCelsius x = (x - 32)/1.8

haceFrioF :: Float -> Bool
haceFrioF x | x < 46.4 = True
            | otherwise = False

--TUPLAS--

segundo3 :: (Int, Int, Int) -> Int
segundo3 (a, b, c) = b

ordena :: (Int, Int) -> (Int, Int)
ordena (a, b) | a <= b = (a, b)
              | otherwise  = (b, a)

rangoPrePar :: Int -> (Int, Int) -> String
rangoPrePar a (b, c) | 0 < a && a < b && a < c = "Muy Barato"
                     | a > b && a > c = "Muy Caro"
                     | a >= b && a <= c = "Hay que verlo"
                     | a < 0 = "No puede ser"

mayor3 :: (Int, Int, Int) -> (Bool, Bool, Bool)
mayor3 (a, b, c) = (a > 3, b > 3, c > 3)

todosIguales :: (Int, Int, Int) -> Bool
todosIguales (a, b, c) = a == b && b == c


--PRÁCTICO 2--
--FUNCIONES RECURSIVAS--

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x:soloPares xs
                 | mod x 2 == 1 = soloPares xs

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | x > 10  = x:mayoresQue10 xs
                    | x <= 10 = mayoresQue10 xs

mayoresQueY :: Int -> [Int] -> [Int]
mayoresQueY _ [] = []
mayoresQueY y (x:xs) | y < x  = x:mayoresQueY y xs
                     | y >= x = mayoresQueY y xs

sumar1 :: [Int] -> [Int]
sumar1 [] = []
summar1 (x:xs) = (1+x):sumar1 xs

duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = (2*x):duplicar xs

multiplica :: Int -> [Int] -> [Int]
multiplica _ [] = []
multiplica n (x:xs) = (n*x):multiplica n xs

todosMenores :: [Int] -> Bool
todosMenores [] = True
todosMenores (x:xs) = (x<10) && todosMenores xs

hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) = (x==0) || hay0 xs

summ :: [Int] -> Int
summ [] = 0
summ (x:xs) = x + summ xs

sumaPares :: [(Int,Int)] -> Int
sumaPares [] = 0
sumaPares ((a,b):xs) = (a+b) + sumaPares xs

todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) = ((x==0) || (x==1)) &&todos0y1 xs



quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) | (x==0) = quitar0s xs
                | otherwise = x:quitar0s xs

ultimo :: [a] -> a 
ultimo [x] = x
ultimo (y:x:xs) = ultimo xs



