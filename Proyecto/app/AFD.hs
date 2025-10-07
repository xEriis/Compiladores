module AFD

(
    AFD(..),
    Trans_afd,
    minimiza
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

--La estructura de un Automata Finito Determinista.
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

-- Función que elimina los estados no alcanzables desde el estado inicial
-- del automata recibido.
-- Recibe un AFD y regresa un AFD sin los estados no alcanzables, ni 
-- sus tranciones.
eliminaInalcanzables :: AFD -> AFD
eliminaInalcanzables (AFD q a d i f) = (AFD alcanzables a (eliminaTransiciones d noAlcanzables) i (f \\ noAlcanzables))
                        where alcanzables = getAlcanzables [i] d
                              noAlcanzables = q \\ alcanzables 

-- Función auxiliar para eliminar las transiciones de los estados no alcanzables
-- funciona haciendo uso de listas de comprensión de manera que solo agrega aquellas transiciones 
-- que no tienen alguno de sus estados en la tupla contenidos en los estados no alcanzables.
-- Recibe el mapeo de transiciones y una lista de los estados no alcanzables
-- Regresa el nuevo mapeo de transiciones, sin las transiciones que involucraban
-- estados no alcanzables.
eliminaTransiciones :: [Trans_afd] -> [String] -> [Trans_afd]
eliminaTransiciones d s = [ (qb, t, qn) | (qb, t, qn) <- d, not (qb `elem` s || qn `elem` s)]

-- Función auxiliar que obtiene los estados alcanzables dada una lista de estados
-- Recibe una lista de estados y el mapeo de transiciones.
-- Regresa una lista con los estados alcanzables desde cada 
-- uno de los estados en la lista.
-- Se usa inicialmente desde el estado inicial.
getAlcanzables :: [String] -> [Trans_afd] -> [String]
getAlcanzables q d
  | null ep || esSubLista epn qp = q
  | otherwise = getAlcanzables (nub (qp ++ epn)) d
  where ep = estadosPosiblesLista q d
        epn = nub ep
        qp = nub q

-- Función auxiliar que identifica si una lista es sublista de otra
-- similar a la funcionalidad de estar contenido en de los conjuntos.
esSubLista :: Eq a => [a] -> [a] -> Bool
esSubLista xs ys = all (\x -> elem x ys) xs

-- Función auxiliar para obtener los estados alcanzables que 
-- regresa todos los estados posibles dada una lista de estados
-- Recibe una lista de estados y el mapeo de transiciones
-- Regresa una lista de estados
estadosPosiblesLista :: [String] -> [Trans_afd] -> [String]
estadosPosiblesLista [] d = []
estadosPosiblesLista (q:qs) d = [q] ++ qp ++ estadosPosiblesLista qs d
  where qp = estadosPosibles q d

-- Función auxiliar para obtener los estados alcanzables dado
-- un estado, similar a la versión de lista solo que para un solo
-- elemento.
-- Recibe un estado y el mapeo de transiciones 
-- Regresa una lista de estados
estadosPosibles :: String -> [Trans_afd] -> [String]
estadosPosibles q [] = []
estadosPosibles q (t:ts) 
  | q0 == q = [qn] ++ (estadosPosibles q ts)
  | otherwise = estadosPosibles q ts
  where (q0, ms, qn) = t

-- La función principal de minimización.
-- Primero elimina los estados no alcanzables, luego se le aplica el
-- algoritmo de minimización visto en clase y luego se unen y mezclan
-- los estados nuevos equivalentes obtenidos de la minimización.
-- Esta implementación hace uso de los estados equivalentes en su forma de string
-- es decir, que si tenemos "q0" y "q1" como equivalentes, su representación será
-- "q0q1" de manera que para modificar los estados del automata original, sus transiciones
-- su estado inicial y su estado final hará falta comparar substrings de los estados y poder
-- remplazarlos o en su caso eliminarlos, por sus equivalentes ya mezclados o unidos.
-- Hace uso de varias funciones auxiliares para poder funcionar correctamente.
-- Recibe un AFD y regresa un AFD.
minimiza :: AFD -> AFD
minimiza (AFD q a d i f) = (AFD (minimizaEstados nq eqs) na (nub (minimizaTransiciones nd [] eqs)) (minimizaInicial ni eqs) (nub (minimizaFinales nf eqs)))
  where lq = length q
        (AFD nq na nd ni nf) = eliminaInalcanzables (AFD q a d i f)
        nlq = length nq
        eq = estadosEquivalentes nq (minimizaAux (AFD nq na nd ni nf) (matrix nlq nlq $ \(i,j) -> -1))
        equ = uneEstados eq eq
        eqs = [concat s | s <- equ]


-- Función auxiliar para minimizar un AFD 
-- Recibe un AFD, una matriz que representa nuestra tabla y le aplica el algoritmo
-- visto en clase, es decir, los primeros dos pasos del algoritmo, con una matriz de -1
-- aunque podría cambiarse el valor a cualquier otro realmente.
-- Regresa una matriz despues de haberse aplicado el algoritmo, lo que devuelve
-- los estados equivalentes representados por un 0 en su casilla.
minimizaAux :: AFD -> Matrix Int -> Matrix Int
minimizaAux a m = (operaTabla2 a m (operaTabla1 a m))

-- Función auxiliar para poder obtener el estado inicial de un automata con 
-- estados equivalentes ya encontrados. Hace uso de comparación de substrings
-- Recbie el estado incial y una lista de estados ya unidos.
-- Regresa el nuevo estado inicial si es que el estado inicial original estaba
-- contenido entre los estados equivalentes obtenidos del algoritmo de minimización
minimizaInicial :: String -> [String] -> String
minimizaInicial i eq = if null ni then i else ni
                     where ni = getString i eq

-- Función auxiliar usada en la minimización de automatas. Hace uso de comparación y
-- contención de substrings para poder obtener y/o eliminar un estado que ya estaba 
-- en la lista de los nuevos estados mezclados, esto es, que el estado "qi" se elimina
-- para ser emplazado por "qiqi+1..." en caso de que dicha equivalencia se haya encontrado.
-- Recibe una lista de estados del automata original y la lista de automatas mezclados
-- Regresa los nuevos estados del automata usando como referencia los estados ya mezclados
-- y los que no fueron equivalentes.
minimizaEstados :: [String] -> [String] -> [String]
minimizaEstados [] nq = nq
minimizaEstados (q:qs) nq 
  | contieneString q nq = (minimizaEstados qs nq)
  | otherwise = minimizaEstados qs ([q] ++ nq)

-- Función auxiliar usada en la minimización de automatas. Similar a la función minimizaInicial
-- el objetivo de esta función es obtener los nuevos estados finales obtenidos de la mezcla
-- y juntarlos con los que ya eran finales y no resultaron en ninguna equivalencia.
-- Recibe una lista de estados finales y los estados equivalentes
-- Regresa una lista con los nuevos estados finales.
minimizaFinales :: [String] -> [String] -> [String]
minimizaFinales [] nq = []
minimizaFinales (q:qs) nq 
  | null ns = [q] ++ (minimizaFinales qs nq)
  | otherwise = [ns] ++ minimizaFinales qs nq
  where ns = getString q nq

-- Función auxiliar usada en la minimización del algoritmo. El objetivo es cambiar
-- los estados de las transiciones por sus equivalentes mezclados obtenidos del algoritmo
-- de minimización, haciendo uso de comparación de substrings. Es decir, que si por ejemplo
-- se tiene una transición de la forma (q0, a, q1) y "q0q1" resultó ser un nuevo estado
-- entonces dicha transición pasará a ser de la forma (q0q1, a, q0q1).
-- Recibe la lista de mapeo de transiciones el automata original y 
-- otra adicional donde se irán guardando las nuevas transiciones, además recibe la 
-- lista de estados equivalentes.
-- Regresa una lista que representa el mapeo de transiciones del automata minimizado.
minimizaTransiciones :: [Trans_afd] -> [Trans_afd] -> [String] -> [Trans_afd]
minimizaTransiciones [] nd _ = nd
minimizaTransiciones (d:ds) nd s = minimizaTransiciones ds (nd ++ [(qbn, dt, qnn)]) s
  where (qb, dt, qn) = d
        subB = getString qb s
        subN = getString qn s
        qbn = if null subB then qb else subB 
        qnn = if null subN then qn else subN

-- Función auxiliar usada para obtener el string de estados mezclados al que 
-- pertence un estado. Se hace caso de un case ya que puede no pertenecer a
-- ninguno de strings dados en la lista, además se hace uso de una lista por
-- comprensión en el que se obtiene todos los strings con los que hace match
-- nuestro string s1, pero por la forma en la que están diseñadas nuestras 
-- funciones y automatas, esto solo devolverá una lista con un solo elemento 
-- o la lista vacía en caso de que no haya matches, por lo que en ese caso
-- se devuelve un string vacío para poder ser manejado en otras funciones
-- que hagan uso de esta función.
-- Recibe un estado en forma de string y una lista de estados.
-- Regresa el estado al cuál pertenece un estado (e.g. q0 in q0q1)
getString :: String -> [String] -> String
getString s1 s2 = case [x | x <- s2, s1 `isInfixOf` x] of 
                (x:_) -> x 
                [] -> ""

-- Función auxiliar similar a getString, con la única diferencia que esta solo
-- devuelve un booleano en caso de que haya encontrado como substring de algún 
-- string de ls.
-- Recibe un estado y una lista de estados mezclados
-- Regresa True en caso de que haya un match y no en otro caso.
contieneString :: String -> [String] -> Bool 
contieneString s1 ls = any (s1 `isInfixOf`) ls

-- Función auxiliar que une los estados obtenidos del algoritmo de minimización,
-- es decir, obtiene una lista de listas con los estados equivalentes, haciendo
-- uso de nub y sort para poder eliminar duplicados dados por las operaciones
-- recursivas hechas en uneListasIntersectadas.
-- Recibe una lista de listas de estados y otra con los pares de listas de estados
-- equivalentes. Regresa una lista de listas de strings con los pares equivalentes
-- (e.g puede regresa [[q0, q1], [q3, q4, q5]])
uneEstados:: [[String]] -> [[String]] -> [[String]]
uneEstados [] _ = []
uneEstados (q:qs) eq = nubSet ([nuevoQ] ++ uneEstados qs eq)
                  where nuevoQ = sortSet (nubSet (uneListasIntersectadas q eq))

-- Función auxiliar usada para construir el automata minimizado, usada para 
-- obtener todos los estados equivalentes y posteriormente poder mezclarlos
-- se hace uso de insterct para obtener que pares de estados están en otra equivalencia
-- y poder ir obteniendo los estados con los que otros están relacionados entre sí
-- (e.g [[ab, bc], [bc, bd]] => [ab,bc,bd]) Adicionalmente se hace uso de nubSet en lugar
-- de nub para un mejor performance de la función (puede que esto cambie).
-- Recibe una lista de estados y la lista de los pares de estados equivalentes.
-- Regresa una lista de estados con los que se es equivalente.
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

-- Automatas de prueba, se quitarán posteriormente
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
