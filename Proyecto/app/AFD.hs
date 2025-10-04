module AFD

(
    AFD(..),
    Trans_afd
)

where

import Data.List (elemIndex)
import Data.Matrix

type Trans_afd = (String, Char, String)
data AFD = AFD {
    estadosD :: [String],
    alfabetoD :: [Char],
    transicionesD :: [Trans_afd],
    inicialD :: String,
    finalesD :: [String]
} deriving (Show)

-- Recibe un estado, una cadena de simbolos y la lista de transiciones
-- Regresa el estado final al que llega o en el que se detiene
transita :: String -> String -> [Trans_afd] -> String 
transita q [] _ = q
transita q (s:ss) d    
  | null qn = q
  | otherwise = transita qn ss d
  where qn = checaTransicion q s d

-- Recibe un estado, un simbolo y la lista de transiciones 
-- Regresa el estado encontrado o ninguno si no hubo match
checaTransicion :: String -> Char -> [Trans_afd] -> String
checaTransicion _ _ [] = []
checaTransicion q s (x:xs)
  | q == qo && s == ts = qn
  | otherwise = checaTransicion q s xs
  where (qo, ts, qn) = x

-- Solamente checa si transita regresa un estado final
acepta :: String -> AFD -> Bool
acepta s (AFD _ _ d q0 f) = transita q0 s d `elem` f

-- Todavía haciendose, solo regresa un [[String]] para motivos de debuggin
minimiza :: AFD -> [[String]]
minimiza (AFD q a d i f) = estadosEquivalentes q (minimizaAux (AFD q a d i f) (matrix lq lq $ \(i,j) -> -1))
  where lq = length q

-- Función auxiliar para minimizar un AFD 
-- Recibe un AFD, una matriz que representa nuestra tabla y regresa "por ahora"
-- una tabla
minimizaAux :: AFD -> Matrix Int -> Matrix Int
minimizaAux a m = operaTabla2 a m (operaTabla1 a m) 

-- El primer paso del algoritmo de minimización provisto por Diego
-- Checa si alguno de los dos estados correspondientes a una casilla 
-- son iguales o no, si sí se marca con 1, si no se marca con un 0
-- Recibe un AFD una matriz de enteros que representa la tabla
-- Regresa una matriz de enteros modificada por el primer paso
operaTabla1 :: AFD -> Matrix Int -> Matrix Int
operaTabla1 (AFD q _ _ _ f) table = mapPos(\(i,j) x -> if (i < length q) && (condicion1 f (q !! (j-1)) (q !! (i))) && j <= i then 1 else 0) table

-- Función auxiliar para el primer paso del algoritmo de minimización
-- Esta función se encarga de revisar la primera condición, es decir 
-- regresa True si uno es final y el otro no, False en otro caso
condicion1 :: [String] -> String -> String -> Bool
condicion1 f j i = ((j `elem` f) && not (i `elem` f)) || (not (j `elem` f) && (i `elem` f))

-- Función auxiliar que realiza el segundo paso del algoritmo de minimización
-- Recibe un AFD, la matriz y una matriz nueva modificada por el algoritmo,
-- esto se hace para comprobar si es que ya no hay cambios en la tabla, lo 
-- que quiere decir que ya se encontraron los estados equivalentes
operaTabla2 :: AFD -> Matrix Int -> Matrix Int -> Matrix Int
operaTabla2 (AFD q a d i f) table newTable
  | table == newTable = table
  | otherwise = operaTabla2 (AFD q a d i f) newTable (operaTabla2Aux q a d newTable)

-- Función auxiliar para la segunda operación del algoritmo de minimización
-- Se encarga de revisar cada casilla con 0 en una iteración del algoritmo y 
-- revisa con todos los caracteres del alfabeto. 
-- Recibe la lista de estados, el alfabeto, el mapeo de transiciones y la matriz 
-- a modificar. 
-- Regresa una matriz modificada.
operaTabla2Aux :: [String] -> [Char] -> [Trans_afd] -> Matrix Int -> Matrix Int
operaTabla2Aux _ [] _ m = m
operaTabla2Aux q (c:cs) d m = operaTabla2Aux q cs d (mapPos(\(i,j) x -> if (x == 0) && (i < length q) && (j <= i) && (checaCasilla q (checaTransicion (q !! (j-1)) c d) (checaTransicion (q !! i) c d) m) then 1 else x) m)

-- Función auxiliar para la función auxiliar de la segunda operación del algoritmo
-- de minimización, esta se encarga de revisar la condición dada por el algoritmo
-- es decir, revisar la función de transición y revisar si la casilla resultante
-- ya está marcada con un 1 en nuestro caso, si sí, se regresa True para que la 
-- casilla en la iteración actual se marque, si no, se regresa False
checaCasilla :: [String] -> String -> String -> Matrix Int -> Bool
checaCasilla q j i m 
  | null i || null j = False
  | otherwise = (j /= i) && (m ! (maxIdx, minIdx+1) == 1)
  where 
    Just qi = elemIndex i q 
    Just qj = elemIndex j q
    qin = if qi == 0 then qi + 1 else qi 
    qjn = if qj == 0 then qj + 1 else qj 
    minIdx = min qin qjn 
    maxIdx = max qin qjn

-- Función que obtiene los estados equivalentes que encuentra el algoritmo
-- de minimización visto en clase, funciona obtiendo todos las posiciones
-- de la matriz en una lista de posiciones, luego las filtra asegurandose
-- que se quede con las que quedaron en 0 y además sea del triangulo
-- derecho de la matriz (i.e. j <= i) y que i < length q por como obtenemos
-- los estados apropiados dado el algoritmo, por último solo obtenemos los 
-- estados correspondientes dadas las posiciones encontradas.
estadosEquivalentes :: [String] -> Matrix Int -> [[String]]
estadosEquivalentes q m =
  let
    listaCoordenadas = [(i, j) | i <- [1..nrows m], j <- [1..ncols m]]
    equivalentes = filter (\(i, j) ->
      let x = m ! (i,j)
      in x == 0 && j <= i && i < length q
      ) listaCoordenadas
  in map (\(i, j) -> [q !! (j-1), q !! i]) equivalentes

automata3States :: AFD
automata3States = AFD {
    estadosD = ["A", "B", "C"],
    alfabetoD = ['0', '1'],
    transicionesD = [
        -- {Current, Input, Next}
        ("A", '0', "B"), -- {A, 0, B}
        ("A", '1', "C"), -- {A, 1, C}
        ("B", '0', "B"), -- {B, 0, B}
        ("B", '1', "C"), -- {B, 1, C}
        ("C", '0', "B"), -- {C, 0, B}
        ("C", '1', "C")  -- {C, 1, C}
    ],
    inicialD = "A",
    finalesD = ["C"]
}

a2 :: AFD
a2 = AFD {
    estadosD = ["q0", "q1", "q2", "q3", "q4", "q5"],
    alfabetoD = ['a', 'b'],
    transicionesD = [
        -- (Current State, Input, Next State)
        ("q0", 'a', "q1"),
        ("q0", 'b', "q2"),
        ("q1", 'a', "q0"),
        ("q1", 'b', "q3"),
        ("q2", 'a', "q4"),
        ("q2", 'b', "q5"),
        ("q3", 'a', "q4"),
        ("q3", 'b', "q5"),
        ("q4", 'a', "q4"),
        ("q4", 'b', "q5"),
        ("q5", 'a', "q5"),
        ("q5", 'b', "q5") 
    ],
    inicialD = "q0",
    finalesD = ["q2", "q3", "q4"]
}

a3 = AFD {
    estadosD = ["q0", "qA", "qB", "qAA", "qAB", "qBA", "qBB", "qDEAD"],
    alfabetoD = ['a', 'b'],
    transicionesD = [
        ("q0", 'a', "qA"),
        ("q0", 'b', "qB"),
        ("qA", 'a', "qAA"),
        ("qA", 'b', "qAB"),
        ("qB", 'a', "qBA"),
        ("qB", 'b', "qBB"),
        ("qAA", 'a', "qAA"), -- Stays finished with 'a'
        ("qAA", 'b', "qAB"), -- Switches to finished with 'b'
        ("qAB", 'a', "qAA"), -- Switches to finished with 'a'
        ("qAB", 'b', "qAB"), -- Stays finished with 'b'
        ("qBB", 'a', "qBA"), -- Stays finished with 'b'
        ("qBB", 'b', "qBB"), -- Switches to finished with 'a'
        ("qBA", 'a', "qBA"), -- Switches to finished with 'b'
        ("qBA", 'b', "qBB"), -- Stays finished with 'a'
        ("qDEAD", 'a', "qDEAD"),
        ("qDEAD", 'b', "qDEAD")
    ],
    inicialD = "q0",
    finalesD = ["qAA", "qAB", "qBA", "qBB"]
}
--marcaCasilla :: Matrix Int -> Matrix Int

-- Elementos del autómata finito determinista
-- type Estado = Int
-- type Simbolo = Char
-- type Delta = Estado -> Simbolo -> Estado
-- -- Quintupla del estado finito determinista
-- data AFD = AFD {
--     estados :: [Estado],
--     alfabeto :: [Simbolo],
--     delta :: Delta,
--     inicial :: Estado,
--     finales :: [Estado]
-- }

-- transita :: String -> Estado -> Delta -> Estado
-- transita [] q _ = q
-- transita (s:cs) q d = transita cs q' d
--    where q' = d q s

-- acepta :: String -> AFD -> Bool
-- acepta s (AFD _ _ d q0 f) = elem(transita s q0 d)f


-- -- Ejemplo de una autómata
-- deltaPar :: Delta
-- deltaPar 0 '0' = 0
-- deltaPar 0 '1' = 1
-- deltaPar 1 '0' = 1
-- deltaPar 1 '1' = 0
-- deltaPar _  _  = error "Símbolo inválido"
-- -- Automata
-- afdPar :: AFD
-- afdPar = AFD { 
--     estados  = [0,1],
--     alfabeto = ['0','1'],
--     delta    = deltaPar,
--     inicial  = 0,
--     finales  = [0]
-- }

-- --Añadir función minimizadora?
