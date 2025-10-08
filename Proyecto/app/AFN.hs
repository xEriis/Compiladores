module AFN
(
    AFN(..),
    Trans_afn,
    afn_to_AFD
)
where

-- import Data.List (nub,sort)
import AFD
type Trans_afn = (String, Char, [String])
data AFN = AFN {
    estadosN :: [String], 
    alfabetoN :: [Char],
    transicionesN :: [Trans_afn],
    inicialN :: String, 
    finalN :: String
    }
  deriving (Show)

afn_to_AFD :: AFN -> AFD
afn_to_AFD afn = AFD {estadosD = estadosAFD,
                    alfabetoD = alfabetoN afn,
                    transicionesD = transicionesAFD,
                    inicialD = inicialAFD,
                    finalesD = finalesAFD
}
  where
    -- Nuevos estados
    recEstadosAFD = generaEstadosAFD afn [[inicialN afn]] []
    estadoSinEmptyAFD = filter (/= ["empty"]) recEstadosAFD
    mapeo = generalLabel estadoSinEmptyAFD
    
    -- Label de los estados
    estadosAFD = map (mapearEstado mapeo) estadoSinEmptyAFD

    -- Transiciones
    transicionesAFD = generarTransicionesAFD afn estadoSinEmptyAFD mapeo

    -- Finales
    finalesAFD = encuentraFinalesAFD afn estadoSinEmptyAFD mapeo

    -- Inicial
    inicialAFD = mapearEstado mapeo [inicialN afn]


-- Metodo principal para generar los estados posibles del AFD explorando el afn
-- Toma 3 parámetros (afn, Lista de estados a los que vamos llegando que estan en Q', Lista de estados que ya agregamos)
-- Regresa una lista de todos los estados posibles (y conjuntos convertidos en un solo estado) del AFD
generaEstadosAFD :: AFN -> [[String]] -> [[String]] -> [[String]]
generaEstadosAFD _ [] agregados = agregados
generaEstadosAFD afn (current:resto) agregados
    | current `elem` agregados = generaEstadosAFD afn resto agregados -- si llegamos a un estado visitado ya no lo volvemos a checar
    | otherwise = let 
                      -- Para cada simbolo (del alfabeto) vemos a que estado llegamos desde el actual
                      -- Nuevos estados a los que llegamos
                      nuevosEstados = [destino | 
                                       simbolo <- alfabetoN afn,
                                       let rDestino = transicionAux afn current simbolo,
                                       let destino = if null rDestino || rDestino == ["empty"] -- Si queremos que exita el estado muerto entonces 
                                                     then [] 
                                                     else rDestino,
                                       not (null destino)]
                      noAgregados = filter (`notElem` (current:resto ++ agregados)) nuevosEstados
                  in generaEstadosAFD afn (resto ++ noAgregados) (current:agregados)

-- Metodo para generar las transiciones del AFD dados los únicos estados posibles que tendrá (sin calcular el conjunto potencia del afn)
generarTransicionesAFD ::AFN -> [[String]] -> [([String], String)] -> [Trans_afd]
generarTransicionesAFD afn estadosAFD mapeo =
    [ (mapearEstado mapeo origen, simbolo, mapearEstado mapeo destino)
    | origen <- estadosAFD,
      simbolo <- alfabetoN afn,
      let destino = transicionAux afn origen simbolo,
      let des = if null destino then [] else destino,
      not (null destino)
    ]

-- Método general para renombrar los estados del AFD secuencialmente
generalLabel :: [[String]] -> [([String], String)]
generalLabel estadosAFD = zip estadosAFD nombres
  where
    nombres = [ "q" ++ show i | i <- [0..length estadosAFD - 1] ]

-- Método para encontrar a los estados finales
encuentraFinalesAFD :: AFN -> [[String]] -> [([String], String)] -> [String]
encuentraFinalesAFD afn estadosAFD mapeo =
    [ mapearEstado mapeo conjunto
    | conjunto <- estadosAFD,
      finalN afn `elem` conjunto
    ]

-- Método auxiliar para calcular la transición de un conjunto de estados con un símbolo
transicionAux :: AFN -> [String] -> Char -> [String]
transicionAux afn conjuntoEdos sim = 
  let destinos = eliminarDup [destino | estado <- conjuntoEdos,
                                        destino <- findTrans afn estado sim]

  in if null destinos then [] else destinos

-- Método auxiliar para encontrar las transiciones de UN solo estado con un símbolo 
-- regresa una lista de estados (por ser no determinista)
findTrans :: AFN -> String -> Char -> [String]
findTrans afn estado simbolo = [destino | (origen, sim, destinos) <- transicionesN afn,
                                        origen == estado, sim == simbolo,
                                        destino <- destinos ]
                                      
-- Método auxiliar elimina duplicados de una lista manteniendo el orden
eliminarDup :: [String] -> [String]
eliminarDup [] = []
eliminarDup (x:xs)
    | x `elem` xs = eliminarDup xs
    | otherwise = x : eliminarDup xs

-- Método para renombrar estados con notacion de conjuntos
nombreEstado :: [String] -> String
nombreEstado [] = "empty" -- Aqui al momento de hacer la tabla de transiciones, hay estados que con ese simbolo no van a ningun lado, entonces estan como empty pero siguiendo la notacion pues puse []
nombreEstado ["empty"] = "empty"
nombreEstado estados = "{" ++ separa "," (estados) ++ "}" -- Notacion de los estados como conjuntos

-- Método auxiliar para separar los estados que se convierten en un solo estado del AFD
separa :: String -> [String] -> String
separa _ [] = ""
separa _ [x] = x
separa sep (x:xs) = x ++ sep ++ separa sep xs

-- Función para mapear un estado a su nuevo nombre
mapearEstado :: [([String], String)] -> [String] -> String
mapearEstado mapeo estado 
    | null estado = "empty"
    | otherwise = case lookup estado mapeo of
        Just nombre -> nombre
        Nothing -> nombreEstado estado
