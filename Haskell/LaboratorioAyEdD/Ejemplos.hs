

--Ejemplo de Pattern Matching: La forma "tradicional" de definir una funcion que dada una lista retorne "True" 
--si no tiene ningun elemento es la siguiente:

esVacia :: [Int] -> Bool
esVacia [] = True
esVacia (x:xs) = False

--Si utilizamos Pattern Matching, podemos definirlo de la siguiente manera.

esVacía :: [Int] -> Bool
esVacía [] = True
esVacía (x:[]) = False
esVacía (x:y:xs) = False

--Alto Orden: 
--Una funcion puede tomar a otra funcion como un valor y a partir de esta defino mi nueva funcion.
-- Esto es aplicable ya que tienen el mismo tipo: (esMultiplo) -> (Int -> Bool).

esMultiplo :: Int -> Int -> Bool
esMultiplo m n = mod n m == 0
esPar :: Int -> Bool
esPar = esMultiplo 2
esImpar :: Int -> Bool
esImpar = esMultiplo 3

--Definiciones de Tipos de Datos:
--Datos Enumerados: Se utilizan para enumerar cada uno de los valores del tipo.
--Cada uno de dichos valores es un constructor, ya que al utilizarlos en una expresión, generan un valor del tipo "Carrera".

data Carrera = Matemática | Física | Computación | Astronomía

titulo :: Carrera -> String
titulo Matemática = "Licenciatura en Matemática"
titulo Física = "Licenciatura en Fisica"
titulo Astronomía = "Licenciatura en Astronomía"
titulo Computación = "Licenciatura en ciencias de la Computación"

--Clases de Tipos:
--Una clase en un conjunto de tipos que proveen ciertas operaciones especiales (Eq, Ord, Bounded, Show, etc).
--Utilizando el ejemplo anterior, aplicamos show para comparar dicha función.

data Carreras = Matemáticas | Físicas | Computacines | Astronomias deriving Eq

--Polimorfismo ad hoc: Es una forma de restringir el polimorfismo de la variable a una clase.

sumatoriaLiteral :: Num a => [a] -> a
sumatoriaLiteral [] = 0
sumatoriaLiteral (x:xs) = x + sumatoriaLiteral xs

--Sinónimos de Tipos / Constructores con Parámetros:
--Los tipos Algebráicos tienen constructores que llevan parámetros. Estos pa´rametros permiten agregar información.

--Sinónimos de Tipos:
type Altura = Int
type NumCamiseta = Int
--Tipos Algebráicos sin Parámetros:
data Zona = Arco | Defensa | Mediocampo | Delantera
data Modalidad = Carretera | Pista | Monte 
data PiernaHábil = Izquierda | Derecha
data TipoRevés = DosManos | UnaMano
--Sinónimo:
type ManoHábil = PiernaHábil
--Deportista es un tipo Algebráico con constructones Parampetricos:
data Deportista = Ajedrecista                                         -- Constructor sin Argumentos
                | Ciclista Modalidad                                  -- Constructor con un Argumento
                | Velocidad Altura                                    -- Constructor con un Argumento
                | Tenista TipoRevés ManoHábil Altura                  -- Constructor con tres Argumentos
                | Futbolista Zona NumCamiseta PiernaHábil Altura      -- constructor con 4 Argumentos

--Tipos Enumerados con Polimorfismo

dividir :: Int -> Int -> Maybe Int
dividir x 0 = Nothing
dividir x y = Just (div x y)

--Este tipo Maybe es dependiente de de la variable a, en concecuenia Maybe es un constructor de tipos polimórficos.

