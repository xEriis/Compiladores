module AFD

(
    AFD(..),
    Trans_afd
)

where

import Data.List (elemIndex, nub, intersect, (\\), sort, isInfixOf)
-- Se tiene que importar así porque si no, hay un conflicto con el import de 
-- Data.List ya que tienen funciones llamadas igual
import qualified Data.Set as Set
import Data.Matrix

-- Función auxiliar para mejorar la complejidad de quitar elementos dublicados
-- en una lista, lo convierte a un conjunto para usar un arbol
nubSet :: Ord a => [a] -> [a]
nubSet = Set.toList . Set.fromList

-- Función auxiliar para mejorar la complejidad de ordenar una lista al convertilo
-- a un conjunto, es decir, usa árboles
sortSet :: Ord a => [a] -> [a]
sortSet = Set.toAscList . Set.fromList

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

eliminaInalcanzables :: AFD -> AFD
eliminaInalcanzables (AFD q a d i f) = (AFD alcanzables a (eliminaTransiciones d noAlcanzables) i (f \\ noAlcanzables))
                        where alcanzables = getAlcanzables [i] d
                              noAlcanzables = q \\ alcanzables 

eliminaTransiciones :: [Trans_afd] -> [String] -> [Trans_afd]
eliminaTransiciones d s = [ (qb, t, qn) | (qb, t, qn) <- d, not (qb `elem` s || qn `elem` s)]

getAlcanzables :: [String] -> [Trans_afd] -> [String]
getAlcanzables q d
  | null ep || esSubLista epn qp = q
  | otherwise = getAlcanzables (nub (qp ++ epn)) d
  where ep = estadosPosiblesLista q d
        epn = nub ep
        qp = nub q

esSubLista :: Eq a => [a] -> [a] -> Bool
esSubLista xs ys = all (\x -> elem x ys) xs

estadosPosiblesLista :: [String] -> [Trans_afd] -> [String]
estadosPosiblesLista [] d = []
estadosPosiblesLista (q:qs) d = [q] ++ qp ++ estadosPosiblesLista qs d
  where qp = estadosPosibles q d

estadosPosibles :: String -> [Trans_afd] -> [String]
estadosPosibles q [] = []
estadosPosibles q (t:ts) 
  | q0 == q = [qn] ++ (estadosPosibles q ts)
  | otherwise = estadosPosibles q ts
  where (q0, ms, qn) = t

-- Todavía haciendose, solo regresa un [[String]] para motivos de debugging
minimiza :: AFD -> AFD
minimiza (AFD q a d i f) = (AFD (minimizaEstados nq eqs) na (nub (minimizaTransiciones nd [] eqs)) (minimizaInicial ni eqs) (nub (minimizaFinales nf eqs)))
  where lq = length q
        (AFD nq na nd ni nf) = eliminaInalcanzables (AFD q a d i f)
        nlq = length nq
        eq = estadosEquivalentes nq (minimizaAux (AFD nq na nd ni nf) (matrix nlq nlq $ \(i,j) -> -1))
        equ = uneEstados eq eq
        eqs = [concat s | s <- equ]


-- Función auxiliar para minimizar un AFD 
-- Recibe un AFD, una matriz que representa nuestra tabla y regresa "por ahora"
-- una tabla
minimizaAux :: AFD -> Matrix Int -> Matrix Int
minimizaAux a m = (operaTabla2 a m (operaTabla1 a m))

minimizaInicial :: String -> [String] -> String
minimizaInicial i eq = if null ni then i else ni
                     where ni = getString i eq

minimizaEstados :: [String] -> [String] -> [String]
minimizaEstados [] nq = nq
minimizaEstados (q:qs) nq 
  | contieneString q nq = (minimizaEstados qs nq)
  | otherwise = minimizaEstados qs ([q] ++ nq)

minimizaFinales :: [String] -> [String] -> [String]
minimizaFinales [] nq = []
minimizaFinales (q:qs) nq 
  | null ns = [q] ++ (minimizaFinales qs nq)
  | otherwise = [ns] ++ minimizaFinales qs nq
  where ns = getString q nq


minimizaTransiciones :: [Trans_afd] -> [Trans_afd] -> [String] -> [Trans_afd]
minimizaTransiciones [] nd _ = nd
minimizaTransiciones (d:ds) nd s = minimizaTransiciones ds (nd ++ [(qbn, dt, qnn)]) s
  where (qb, dt, qn) = d
        subB = getString qb s
        subN = getString qn s
        qbn = if null subB then qb else subB 
        qnn = if null subN then qn else subN

getString :: String -> [String] -> String
getString s1 s2 = case [x | x <- s2, s1 `isInfixOf` x] of 
                (x:_) -> x 
                [] -> ""

contieneString :: String -> [String] -> Bool 
contieneString s1 ls = any (s1 `isInfixOf`) ls


uneEstados:: [[String]] -> [[String]] -> [[String]]
uneEstados [] _ = []
uneEstados (q:qs) eq = nubSet ([nuevoQ] ++ uneEstados qs eq)
                  where nuevoQ = sortSet (nubSet (uneListasIntersectadas q eq))
--uneTransiciones (sp:sps) (d:ds)

uneListasIntersectadas :: [String] -> [[String]] -> [String]
uneListasIntersectadas a [] = []
uneListasIntersectadas a (x:xs)
  | null i = uneListasIntersectadas a xs
  | otherwise = na ++ nubSet (uneListasIntersectadas na xs)
  where i = a `intersect` x
        na = nubSet (a ++ (x \\ a))

-- El primer paso del algoritmo de minimización provisto por Diego
-- Checa si alguno de los dos estados correspondientes a una casilla 
-- son iguales o no, si sí se marca con 1, si no se marca con un 0
-- Recibe un AFD una matriz de enteros que representa la tabla
-- Regresa una matriz de enteros modificada por el primer paso
-- Nota: en las matrices de Haskell los indices comienzan en (1,1)
-- así que para obtener los estados correspondientes en la lista 
-- de estados se usa j-1 e i sin que i se salga del length de la 
-- lista de estados, esto para hacer las modificaciones de manera correcta
-- y acorde a la tabla de equivalencias del algoritmo visto en clase
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
-- izquierdo de la matriz (i.e. j <= i) y que i < length q por como obtenemos
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

a1 :: AFD
a1 = AFD {
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

au :: AFD
au = AFD {
    estadosD = ["q0", "q1", "q2", "q3", "qUNR_A", "qUNR_B", "qUNR_C"],
    alfabetoD = ['a', 'b'],
    transicionesD = [
        -- REACHABLE COMPONENT (Language: a...a, |w| >= 3)
        ("q0", 'a', "q1"),
        ("q0", 'b', "q3"), -- Trap on 'b' because it must start with 'a'

        ("q1", 'a', "q2"),
        ("q1", 'b', "q2"),

        ("q2", 'a', "q3"), -- Final condition: Saw final 'a'
        ("q2", 'b', "q2"), -- Loop until final 'a' is seen

        ("q3", 'a', "q3"), -- Loop: Stays final on 'a'
        ("q3", 'b', "q2"), -- Switches back to intermediate state on 'b'

        -- UNREACHABLE COMPONENT 1 (Simple Dead State)
        ("qUNR_A", 'a', "qUNR_A"),
        ("qUNR_A", 'b', "qUNR_A"),

        -- UNREACHABLE COMPONENT 2 (Two interconnected states)
        ("qUNR_B", 'a', "qUNR_C"),
        ("qUNR_B", 'b', "qUNR_B"),
        ("qUNR_C", 'a', "qUNR_B"),
        ("qUNR_C", 'b', "qUNR_C")
    ],
    inicialD = "q0",
    finalesD = ["q3", "qUNR_B"]
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
