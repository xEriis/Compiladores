module AFN
(
    AFN(..),
    Trans_afn,
    afn_to_AFD,
    afn_ejemplo3 -- quitar al final
)
where

-- Elementos del autómata finito no determinista
-- BIBLIOTECAS (están comentadas para que no generen warning)

-- import Data.List (nub,sort)
import AFD
-- | Abstracción de una transición no determinista, 
-- igual que la anterior, sin considerar epsilón, pero
-- sigue siendo no determinista. 
type Trans_afn = (String, Char, [String]) -- delta en nuestra definición
data AFN = AFN {
    estadosN :: [String], 
    alfabetoN :: [Char],
    transicionesN :: [Trans_afn],
    inicialN :: String, 
    finalN :: String
    }
  deriving (Show)

-- Como anotación (En teoría podríamos mandar a llamar las funciones para no utilizar el where)
-- Ahora el AFD está bien definido para que @Victor pueda hacer la implementación de AFD a AFD minimo con los datos esperados
afn_to_AFD :: AFN -> AFD
afn_to_AFD afn = AFD {estadosD = estadosAFD,
                    alfabetoD = alfabetoN afn,
                    transicionesD = transicionesAFD,
                    inicialD = nombreEstado [inicialN afn],
                    finalesD = finalesAFD
}
  where
    -- Nuevos estados
    recEstadosAFD = generaEstadosAFD afn [[inicialN afn]] []
    
    -- * Label de los estados
    estadosAFD = map nombreEstado recEstadosAFD

    -- Transiciones
    transicionesAFD = generarTransicionesAFD afn recEstadosAFD

    -- Finales
    finalesAFD = encuentraFinalesAFD afn recEstadosAFD


-- Metodo para generar los estados posibles del AFD explorando el afn
-- Toma 3 parámetros (afn, Lista de estados a los que vamos llegando que estan en Q', Lista de estados que ya agregamos)
-- Regresa una lista de todos los estados posibles (y conjuntos convertidos en un solo estado) del AFD
generaEstadosAFD :: AFN -> [[String]] -> [[String]] -> [[String]]
generaEstadosAFD _ []  agregados = agregados
generaEstadosAFD afn (current:resto) agregados
    | current `elem` agregados = generaEstadosAFD afn resto agregados -- si llegamos a un estado visitado ya no lo volvemos a checar
    | otherwise = let -- Para cada simbolo (del alfabeto) vemos a que estado llegamos desde el actual
                        -- Nuevos estados a los que llegamos
                        nuevosEstados = [destino | simbolo <- alfabetoN afn,
                                                let destino = transicionAux afn current simbolo, not (null destino)]
                        -- Filtramos los estados no esten en nuestros ya agregados
                        noAgregados = filter (`notElem` (current:resto ++ agregados)) nuevosEstados

                    in generaEstadosAFD afn (resto ++ noAgregados) (current:agregados)

-- Aux
-- Metodo para calcular la transicion que necesitamos en el método anterior para ver a que estado(s) nos lleva, este método acepta un conjunto de estados
transicionAux :: AFN -> [String] -> Char -> [String]
transicionAux afn conjuntoEdos sim = eliminarDup [destino | estado <- conjuntoEdos,
                                                            destino <- findTrans afn estado sim]
-- Aux
-- Metodo para saber la transicion de un estado con un simbolo, regresa una lista de estados (por ser no determinista)
findTrans :: AFN -> String -> Char -> [String]
findTrans afn estado simbolo = [destino | (origen, sim, destinos) <- transicionesN afn,
                                        origen == estado, sim == simbolo,
                                        destino <- destinos ]

-- Aux
-- Elimina duplicados de una lista manteniendo el orden
eliminarDup :: [String] -> [String]
eliminarDup [] = []
eliminarDup (x:xs)
    | x `elem` xs = eliminarDup xs
    | otherwise = x : eliminarDup xs

-- Renombrar estados con notacion de conjuntos
nombreEstado :: [String] -> String
nombreEstado [] = "[]" -- Aqui al momento de hacer la tabla de transiciones, hay estados que con ese simbolo no van a ningun lado, entonces estan como empty pero siguiendo la notacion pues puse []
nombreEstado estados = "{" ++ separa "," (estados) ++ "}" -- Notacion de los estados como conjuntos

-- Une strings con un separador
separa :: String -> [String] -> String
separa _ [] = ""
separa _ [x] = x
separa sep (x:xs) = x ++ sep ++ separa sep xs

-- Metodo para generar las transiciones del AFD dados los únicos estados posibles que tendrá (sin calcular el conjunto potencia del afn)
generarTransicionesAFD :: AFN -> [[String]] ->[Trans_afd]
generarTransicionesAFD afn estadosAFD =
    [ (nombreEstado origen, simbolo, nombreEstado destino)
    | origen <- estadosAFD,
      simbolo <- alfabetoN afn,
      let destino = transicionAux afn origen simbolo,
      not(null origen)
    ]

-- Método para encontrar a los estados finales
encuentraFinalesAFD :: AFN -> [[String]] -> [String]
encuentraFinalesAFD afn estadosAFD =
    [ nombreEstado conjunto
    | conjunto <- estadosAFD,
      finalN afn `elem` conjunto
    ]

-- Ejemplo (0 1* (0+1)) 
ejemplo3 :: [String]
ejemplo3 = ["q0", "q1", "q2"]

transiciones_ejemplo3 :: [Trans_afn]
transiciones_ejemplo3 = [
    ("q0", '0', ["q1"]),
    ("q1", '0', ["q2"]),
    ("q1", '1', ["q1", "q2"])
    ]

-- Crea la instancia del AFN
afn_ejemplo3 :: AFN
afn_ejemplo3 = AFN {
    estadosN = ejemplo3,
    alfabetoN = ['0', '1'],
    transicionesN = transiciones_ejemplo3,
    inicialN = "q0",
    finalN = "q2"
}