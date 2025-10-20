module AFN
(
    AFN(..),
    Trans_afn,
    afn_to_AFD
)
where

import AFD

------------------------------------------------------------
-- Abstracción de una automáta finito no determinista (AFN)
------------------------------------------------------------
type Trans_afn = (String, Char, [String]) 
data AFN = AFN {
    estadosN :: [String], 
    alfabetoN :: [Char],
    transicionesN :: [Trans_afn],
    inicialN :: String, 
    finalN :: String
    }
  deriving (Show)

--------------------------------------------------------------------------------------------------------
-- Función que transforma un AFN a un AFD
-- El alfabeto del AFD será el mismo que el del AFN
-- Los estados del AFD son calculados, posteriormente se identifian los estados finales, iniciales
-- Identificado esto se hace un mapeo de los estados para renombrarlos, por dos razones:
-- 1. Los estados del AFD son subconjuntos de estados del AFN entonces se ven como {q0}, {q2,q3}, etc
-- 2. Se hace un renombre para que los estados se vean como q0, q1, q2 sin un orden en especial.
-- Luego se hace el mapeo para la función de transicion y que tenga los nuevos nombres de los estados.
--------------------------------------------------------------------------------------------------------
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
    mapeo = generalLabel recEstadosAFD
    
    -- * Label de los estados en forma secuencial
    estadosAFD = map (mapearEstado mapeo) recEstadosAFD

    -- Transiciones
    transicionesAFD = generarTransicionesAFD afn recEstadosAFD mapeo

    -- Finales
    finalesAFD = encuentraFinalesAFD afn recEstadosAFD mapeo

    -- Inicial
    inicialAFD = mapearEstado mapeo [inicialN afn]


---------------------------------------------------------------------------------------------------
-- Funcion principal para generar los estados del AFD a partir del AFN
-- Recibe 3 parámetros:
-- - AFN: el autómata no determinista
-- - [[String]]: Lista de estados del AFD que vamos generando (visitando) y que faltan por visita.
--               Esto para no generar todo el conjunto potencia.
-- - [[String]]: Lista de estados del AFD que ya hemos visitado y generado.
---------------------------------------------------------------------------------------------------
generaEstadosAFD :: AFN -> [[String]] -> [[String]] -> [[String]]
-- Caso base: Si ya no hay estados que visitar, regresamos los nuevos estados que estan en 'agregados'
generaEstadosAFD _ []  agregados = agregados
-- Caso recursivo: Tomamos el primer estado de la lista de por visitar y vemos a donde nos lleva con cada símbolo del alfabeto
generaEstadosAFD afn (current:resto) agregados
    -- si llegamos a un estado visitado ya no lo volvemos a checar y checamos el resto
    | current `elem` agregados = generaEstadosAFD afn resto agregados
    -- si no lo hemos visitado, para cada simbolo (del alfabeto) vemos a que estado llegamos desde el actual
    -- si el destino es null lo nombramos como "empty", en caso contrario es el conjunto de estados al que llegamos
    -- simbolo sera por cada uno de los del alfabeto
    -- el destino es: estado(s) que llegamos desde el estado actual con ese simbolo (lo checamos con el auxiliar transicionAux)
    | otherwise = let nuevosEstados = [if null destino then ["empty"] else destino | simbolo <- alfabetoN afn, let destino = transicionAux afn current simbolo]
                      -- Filtra la lista de nuevos estados y solo conservamos los que no están ya en 'resto' ni en 'agregados' ni sea el actual 'current'
                      noVisitados = filter (`notElem` (current:resto ++ agregados)) nuevosEstados
                  -- El nuevo estado pendiente a visitar será el resto (pues ya procesamos la cabeza 'current') más los que no hemos visitado y agregamos 'current' a la lista de estados nuevos 
                  in generaEstadosAFD afn (resto ++ noVisitados) (current:agregados)

-------------------------------------------------------------------------------------  
-- Función principal para generar las transiciones del AFD
-- Recibe 3 parámetros:
-- - AFN: el autómata no determinista
-- - [[String]]: Lista de estados del AFD que hemos generado, visto como conjuntos
-- - [([String], String)]: Mapeo de los estados del AFD a sus nuevos nombres
-- Genera las transiciones del AFD
-------------------------------------------------------------------------------------       
generarTransicionesAFD ::AFN -> [[String]] -> [([String], String)] -> [Trans_afd]
generarTransicionesAFD afn estadosAFD mapeo =
    -- Obtenemos las transiciones originales tomando cada estado del AFD (conjunto de estados del AFN) como origen y cada símbolo del alfabeto
    -- calculamos el estado(s) destino al que llegamos desde el estado origen.
    -- La segunda parte es que con estas transiciones mapeamos los estados, usamos 'mapeo' para buscar el origen y destino para luego sustituirlo solo por un estado y ya no sea un conjunto
    [ (mapearEstado mapeo origen, simbolo, mapearEstado mapeo destino)
    -- Tomamos cada estado del AFD y cada uno de ellos será el origen para calcular la transición
    | origen <- estadosAFD,
    -- Para cada estado de origen iteramos sobre todos los simbolos posibles del AFN
      simbolo <- alfabetoN afn,
    -- Calculamos el estado(s) destino al que llegamos desde el estado origen con el símbolo (usamos transicionAux)
      let destino = transicionAux afn origen simbolo
    ]

-------------------------------------------------------------------------------------------------------------------
-- Función para encontrar los Estados Finales del AFD 
-- Determina qué conjuntos de estados del AFD deben ser marcados como estados finales.
-- Un estado del AFD es final si y solo si el conjunto de estados del AFD que lo representa contiene al menos un estado
-- final del AFN original
-- Recibe 3 parámetros:
-- - AFN
-- - [[String]]: Lista de estados del AFD generados
-- - [([String],String)]: El mapeo que relaciona cada conjunto de estados del AFN con su nuevo nombre en el AFD 
-- Genera una lista con los nombres simples de los estados finales del AFD
-------------------------------------------------------------------------------------------------------------------
encuentraFinalesAFD :: AFN -> [[String]] -> [([String], String)] -> [String]
encuentraFinalesAFD afn estadosAFD mapeo =
    -- Filtrar los conjuntos de estados y luego mapearlos a sus nombres
    [ mapearEstado mapeo conjunto
    -- Iteramos sobre cada estado del AFD
    | conjunto <- estadosAFD,
    -- Aplicamos la condición: incluimos el 'conjunto' solo si contiene AL MENOS UN estado final del AFN.
      finalN afn `elem` conjunto
    ]

------------------------------------------------------------------ 
-- Método para renombrar los estados del AFD de forma secuencial
-- q0, q1, ..., qn
------------------------------------------------------------------
generalLabel :: [[String]] -> [([String], String)]
generalLabel estadosAFD = zip estadosAFD nombres
  where
    -- Genera la lista de nombres secuenciales para los estados, comenzando en "q0".
    nombres = [ "q" ++ show i | i <- [0..length estadosAFD - 1] ]


-----------------------------------------------------------------------------------------------------
-- Función auxiliar de transicion, calcula el conjunto de
-- estados del AFN al que se puede llegar desde cualquier estado al consumir un simbolo específico.
-- Recibe 3 parámetros:
-- - AFN
-- - [String]: conjunto de estados del AFN (ya que para el AFD se genera un neuvo estado con estos)
-- Y verificamos a donde nos lleva cada uno con ese símbolo
-- - Char: sumbolo a consumir
-- Generar [String] el nuevo conjunto de estados a los que se transita (Por ser No determinista)
-----------------------------------------------------------------------------------------------------
-- Metodo para calcular para ver a que estado(s) nos lleva, este método acepta un conjunto de estados
transicionAux :: AFN -> [String] -> Char -> [String]
transicionAux afn conjuntoEdos sim = 
  -- Revisamos cada estado del conjunto pasado como parámetro: 'estado'
  -- para cada 'estado' y el simbolo llamamos la función auxiliar findTrans (devuelve la transición)
  -- la función auxiliar nos devuelve la lista de los estados posibles que llegamos desde 'estado' al consumir 'sim'
  -- el resultado lo vamos almacenando en 'conjuntoEdos', usamos eliminarDup para quitar elementos repetidos
  let destinos = eliminarDup [destino | estado <- conjuntoEdos, destino <- findTrans afn estado sim]
  in if null destinos then ["empty"] else destinos

----------------------------------------------------------
-- Función auxiliar  para saber la transicion de un estado
-- con un simbolo.
-- Genera una lista de estados (por ser no determinista)
----------------------------------------------------------
findTrans :: AFN -> String -> Char -> [String]
findTrans afn estado simbolo = 
  [destino |
    -- Recorremos todas las transiciones del AFN
    (origen, sim, destinos) <- transicionesN afn, 
    -- Filtramos solo las transiciones con estado el origen
    origen == estado,
    -- Filtramos las transicones que se consuman con ese símbolo
    sim == simbolo,
    -- Extraemos cada destino
    destino <- destinos ]

------------------------------------------------------------------------------------------------------
-- Función auxiliar  para mapear un conjunto de estados a su nuevo nombre (notacion de conjuntos).
-- Recibe dos parámetros:
-- -  [([String], String)]: Lista de tuplas que asociara conjunto de estados con su nuevo nombre
-- Por ejemplo si el estados es: {q0} su nuevo nombre es {q0}
-- si el estado es q2,q5,q8 tendrá de nombre: {q2,q5,q8}
-- - [String]: conjunto de estados del AFN que se quiere mapear
-- Genera el nombre correspondiente al conjunto de estados.
------------------------------------------------------------------------------------------------------
mapearEstado :: [([String], String)] -> [String] -> String
mapearEstado mapeo estado = case lookup estado mapeo of
    -- Si el conjunto de estados existe en el mapeo, regresa ese nombre (caso 1 del ejemplo)
    Just nombre -> nombre
    -- Si no existe genera un nombre automático
    Nothing -> nombreEstado estado  

--------------------------------------------------------------------- 
-- Renombrar estados con notacion de conjuntos
nombreEstado :: [String] -> String
nombreEstado [] = "empty" -- Aqui al momento de hacer la tabla de transiciones, hay estados que con ese simbolo no van a ningun lado, entonces estan como empty pero siguiendo la notacion pues puse []
nombreEstado ["empty"] = "empty"
nombreEstado estados = "{" ++ separa "," (estados) ++ "}" -- Notacion de los estados como conjuntos

---------------------------------------------------------------------                    
-- Método auxiliar que elimina duplicados de una lista manteniendo 
-- el orden.
---------------------------------------------------------------------
eliminarDup :: [String] -> [String]
eliminarDup [] = []
eliminarDup (x:xs)
    | x `elem` xs = eliminarDup xs
    | otherwise = x : eliminarDup xs

---------------------------------------------------------------------
-- Método auxiliar para unir  strings con un separador ""
---------------------------------------------------------------------
separa :: String -> [String] -> String
separa _ [] = ""
separa _ [x] = x
separa sep (x:xs) = x ++ sep ++ separa sep xs

