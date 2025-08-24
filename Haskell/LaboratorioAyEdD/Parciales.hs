

--Parcial 1: Tema B

-----------------
-- Ejercicio 1 --
-----------------

type Nombre = String
type Director = String
type Duracion = Int
type Año = Int
type Capitulo = Int
type Temporada = Int

data Video = Pelicula Nombre Director Duracion Año
           | CapSerie Nombre Capitulo Temporada Año  deriving Show

odisea2001 :: Video
odisea2001 = Pelicula "2001: Odisea del espacio" "Stanley Kubrick" 142 1968

theOfficeSO2E05 :: Video
theOfficeSO2E05 = CapSerie "The Office" 5 2 2005

esDelDirector :: Video -> String -> Bool
esDelDirector (CapSerie a y c d) b = False
esDelDirector (Pelicula a y c d) b = y == b

esActual :: Video -> Bool
esActal (CapSerie a b c d) = d >= 2020
esActual (Pelicula a y c d) = False

tiempoPelis :: [Video] -> Int -> Int 
tiempoPelis [] x = 0
tiempoPelis ((Pelicula a b c d):xs) x | x==d = c + tiempoPelis xs x
                                      | otherwise = tiempoPelis xs x 
tiempoPelis((CapSerie a b c d):xs) x = tiempoPelis xs x

-----------------
-- Ejercicio 2 --
-----------------

data ColaVideo = Vacia | Encolada Video ColaVideo deriving Show

seriesActuales :: ColaVideo -> ColaVideo
seriesActuales (Vacia) = Vacia
seriesActuales (Encolada (Pelicula a b c d) cola) = (seriesActuales cola)
seriesActuales (Encolada (CapSerie a b c d) cola) | (esActual (CapSerie a b c d)) = Encolada (CapSerie a b c d) (seriesActuales cola) 



--Parcial 2: Tema D

-----------------
-- Ejercicio 1 --
-----------------

type Nombre0 = String
type Artista = String
type ListaNombreTemas = [String]
type AñoEstreno = Int
type DuracionSeg = Int 

data Lanzamiento = Album Nombre0 Artista ListaNombreTemas AñoEstreno 
                 | Sencillo Nombre Artista DuracionSeg AñoEstreno     deriving Show

thriller :: Lanzamiento
thriller = Album "Thriller" "Michael Jackson" ["Wanna be starting something", "Baby my mine", "The girl is mine"] 1982

yesterday :: Lanzamiento
yesterday = Sencillo "Yesterday" "The Beatles" 125 1965

esDiscoLargo :: Lanzamiento -> Bool 
esDiscoLargo (Album _ _ temas _) = length temas >= 5
esDiscoLargo _ = False 

esSencilloActual :: Lanzamiento -> Bool
esSencilloActual (Sencillo _ _ _ año) = año >= 2020
esSencilloActual _ = False  

tracksAlbumMasLargo :: [Lanzamiento] -> Int
tracksAlbumMasLargo lanzamientos = tracksAlbumMasLargoAux lanzamientos 0
  where
    tracksAlbumMasLargoAux [] maxTracks = maxTracks
    tracksAlbumMasLargoAux (x:xs) maxTracks =
        case x of
            Album _ _ temas _ -> 
                let newMax = max maxTracks (length temas)
                in tracksAlbumMasLargoAux xs newMax
            _ -> tracksAlbumMasLargoAux xs maxTracks

-----------------
-- Ejercicio 2 --
-----------------

data ColaLanzamiento = Vacia0 | Encolada0 Lanzamiento ColaLanzamiento deriving Show

colaReproduccion :: ColaLanzamiento
colaReproduccion = Encolada0 thriller (Encolada0 yesterday Vacia0)

albumsDelArtista :: ColaLanzamiento -> String -> ColaLanzamiento
albumsDelArtista Vacia0 _ = Vacia0 --Caso Base
albumsDelArtista (Encolada0 lanzamiento cola) artista = 
    case lanzamiento of
        Album _ a _ _ | a == artista -> Encolada0 lanzamiento (albumsDelArtista cola artista) 
        -- Si es un álbum del artista, lo incluimos
        _ -> albumsDelArtista cola artista  
        -- Si no, continuamos con el resto de la cola 

