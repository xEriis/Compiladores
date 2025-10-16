module AFD

(
    AFD(..),
    Trans_afd,
    minimiza,
    acepta,
    checaTransicion
)

where

import Data.List (intercalate, elemIndex, nub, intersect, (\\))
import Data.List.Split (splitOn)
-- Se tiene que importar así porque si no, hay un conflicto con el import de 
-- Data.List ya que tienen funciones llamadas igual
import qualified Data.Set as Set
import Data.Matrix
import Data.Char

-- Función auxiliar para mejorar la complejidad de quitar elementos dublicados
-- en una lista, lo convierte a un conjunto para usar un arbol
nubSet :: Ord a => [a] -> [a]
nubSet = Set.toList . Set.fromList

-- Función auxiliar para mejorar la complejidad de ordenar una lista al convertilo
-- a un conjunto, es decir, usa árboles
sortSet :: Ord a => [a] -> [a]
sortSet = Set.toAscList . Set.fromList

-- Nuestra definición de las transiciones de un AFD, la nomenclatura es 
-- (Estado, símbolo, SiguienteEstado)
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
  | q == qo && ts == '#' && s == '1' = qn
  | q == qo && ts == '#' && s == '2' = qn 
  | q == qo && ts == '#' && s == '3' = qn 
  | q == qo && ts == '#' && s == '4' = qn 
  | q == qo && ts == '#' && s == '5' = qn 
  | q == qo && ts == '#' && s == '6' = qn 
  | q == qo && ts == '#' && s == '7' = qn 
  | q == qo && ts == '#' && s == '8' = qn 
  | q == qo && ts == '#' && s == '9' = qn 
  | q == qo && ts == '@' && s1 == 'a' = qn
  | q == qo && ts == '@' && s1 == 'b' = qn 
  | q == qo && ts == '@' && s1 == 'c' = qn 
  | q == qo && ts == '@' && s1 == 'd' = qn 
  | q == qo && ts == '@' && s1 == 'e' = qn 
  | q == qo && ts == '@' && s1 == 'f' = qn 
  | q == qo && ts == '@' && s1 == 'g' = qn 
  | q == qo && ts == '@' && s1 == 'h' = qn 
  | q == qo && ts == '@' && s1 == 'i' = qn 
  | q == qo && ts == '@' && s1 == 'j' = qn 
  | q == qo && ts == '@' && s1 == 'k' = qn 
  | q == qo && ts == '@' && s1 == 'l' = qn 
  | q == qo && ts == '@' && s1 == 'm' = qn 
  | q == qo && ts == '@' && s1 == 'n' = qn 
  | q == qo && ts == '@' && s1 == 'o' = qn 
  | q == qo && ts == '@' && s1 == 'p' = qn 
  | q == qo && ts == '@' && s1 == 'q' = qn 
  | q == qo && ts == '@' && s1 == 'r' = qn 
  | q == qo && ts == '@' && s1 == 's' = qn 
  | q == qo && ts == '@' && s1 == 't' = qn 
  | q == qo && ts == '@' && s1 == 'u' = qn 
  | q == qo && ts == '@' && s1 == 'v' = qn 
  | q == qo && ts == '@' && s1 == 'w' = qn 
  | q == qo && ts == '@' && s1 == 'x' = qn 
  | q == qo && ts == '@' && s1 == 'y' = qn
  | q == qo && ts == '@' && s1 == 'z' = qn 
  | q == qo && s == ts = qn
  | otherwise = checaTransicion q s xs
  where (qo, ts, qn) = x
        s1 = toLower(s)

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
estadosPosiblesLista [] _ = []
estadosPosiblesLista (q:qs) d = [q] ++ qp ++ estadosPosiblesLista qs d
  where qp = estadosPosibles q d

-- Función auxiliar para obtener los estados alcanzables dado
-- un estado, similar a la versión de lista solo que para un solo
-- elemento.
-- Recibe un estado y el mapeo de transiciones 
-- Regresa una lista de estados
estadosPosibles :: String -> [Trans_afd] -> [String]
estadosPosibles _ [] = []
estadosPosibles q (t:ts) 
  | q0 == q = [qn] ++ (estadosPosibles q ts)
  | otherwise = estadosPosibles q ts
  where (q0, _, qn) = t

-- La función principal de minimización.
-- Primero elimina los estados no alcanzables, luego se le aplica el
-- algoritmo de minimización visto en clase y luego se unen y mezclan
-- los estados nuevos equivalentes obtenidos de la minimización.
-- Esta implementación hace uso de los estados equivalentes en su forma de string
-- es decir, que si tenemos "q0" y "q1" como equivalentes, su representación será
-- "q0,q1" de manera que para modificar los estados del automata original, sus transiciones
-- su estado inicial y su estado final hará falta comparar si el estado pertenece al nuevo estado,
-- es decir, comparar si qn está en q0,q1, ..., qn, ..., qi y si ese es el caso entonces
-- remplazarlos o en su caso eliminarlos, por sus equivalentes ya mezclados o unidos.
-- Hace uso de varias funciones auxiliares para poder funcionar correctamente.
-- Recibe un AFD y regresa un AFD.
minimiza :: AFD -> AFD
minimiza (AFD q a d i f) = (AFD (minimizaEstados nq eqs) na (nub (minimizaTransiciones nd [] eqs)) (minimizaInicial ni eqs) (nub (minimizaFinales nf eqs)))
  where (AFD nq na nd ni nf) = eliminaInalcanzables (AFD q a d i f)
        nlq = length nq
        eq = estadosEquivalentes nq (minimizaAux (AFD nq na nd ni nf) (matrix nlq nlq $ \(_,_) -> 0))
        equ = uneEstados eq eq
        eqs = [intercalate "," s | s <- equ]

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
minimizaFinales [] _ = []
minimizaFinales (q:qs) nq 
  | null ns = [q] ++ (minimizaFinales qs nq)
  | otherwise = [ns] ++ minimizaFinales qs nq
  where ns = getString q nq

-- Función auxiliar usada en la minimización del algoritmo. El objetivo es cambiar
-- los estados de las transiciones por sus equivalentes mezclados obtenidos del algoritmo
-- de minimización, haciendo uso de comparación de substrings. Es decir, que si por ejemplo
-- se tiene una transición de la forma (q0, a, q1) y "q0,q1" resultó ser un nuevo estado
-- entonces dicha transición pasará a ser de la forma ("q0,q1", a, "q0q1").
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
getString s1 s2 = case [x | x <- s2, s1 `elem` (splitOn "," x)] of 
                (x:_) -> x 
                [] -> ""

-- Función auxiliar similar a getString, con la única diferencia que esta solo
-- devuelve un booleano en caso de que haya encontrado como substring de algún 
-- string de ls.
-- Recibe un estado y una lista de estados mezclados
-- Regresa True en caso de que haya un match y no en otro caso.
contieneString :: String -> [String] -> Bool 
contieneString s1 ls = any (s1 `elem`) (map (splitOn ",") ls)

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
uneListasIntersectadas _ [] = []
uneListasIntersectadas a (x:xs)
  | null i = uneListasIntersectadas a xs
  | otherwise = na ++ nubSet (uneListasIntersectadas na xs)
  where i = a `intersect` x
        na = nubSet (a ++ (x \\ a))

-- El primer paso del algoritmo de minimización provisto por Diego
-- Checa si alguno de los dos estados correspondientes a una casilla 
-- son iguales o no, si sí se marca con 1, si no se deja la marca que estaba
-- Recibe un AFD una matriz de enteros con valores iniciales igual a 0 que representa la tabla
-- Regresa una matriz de enteros con valores modificada por el primer paso
-- Nota: en las matrices de Haskell los indices comienzan en (1,1)
-- así que para obtener los estados correspondientes en la lista 
-- de estados se usa j-1 e i-1, esto para hacer las modificaciones de manera correcta
-- y acorde a la tabla de equivalencias del algoritmo visto en clase
operaTabla1 :: AFD -> Matrix Int -> Matrix Int
operaTabla1 (AFD q _ _ _ f) table = mapPos (operacion) table
  where
    operacion (i, j) valorActual 
      | j < i && (condicion1 f (q !! (j-1)) (q !! (i-1))) = 1
      | otherwise = valorActual

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
operaTabla2Aux q (c:cs) d m = operaTabla2Aux q cs d (mapPos (operacion) m)
  where
    operacion (i, j) valorActual 
      | valorActual == 0 && j < i && (checaCasilla q (checaTransicion (q !! (j-1)) c d) (checaTransicion (q !! (i-1)) c d) m) = 1
      | otherwise = valorActual

-- Función auxiliar para la función auxiliar de la segunda operación del algoritmo
-- de minimización, esta se encarga de revisar la condición dada por el algoritmo
-- es decir, revisar la función de transición y revisar si la casilla resultante
-- ya está marcada con un 1 en nuestro caso, si sí, se regresa True para que la 
-- casilla en la iteración actual se marque, si no, se regresa False
checaCasilla :: [String] -> String -> String -> Matrix Int -> Bool
checaCasilla q j i m 
  | null i && null j = False
  | null i || null j = True 
  | j == i = False         
  | otherwise = (m ! (maxIdx, minIdx) == 1)
  where 
    qi = case elemIndex i q of
      Just qx -> qx
      Nothing -> -1
    qj = case elemIndex j q of
      Just qy -> qy
      Nothing -> -1
    qin = qi + 1
    qjn = qj + 1
    minIdx = min qin qjn 
    maxIdx = max qin qjn

-- Función que obtiene los estados equivalentes que encuentra el algoritmo
-- de minimización visto en clase, funciona obteniendo todos las posiciones
-- de la matriz en una lista de posiciones, luego las filtra asegurandose
-- que se quede con las que quedaron en 0 y además sea del triangulo
-- izquierdo de la matriz (i.e. j < i). por último solo obtenemos los 
-- estados correspondientes dadas las posiciones encontradas.
estadosEquivalentes :: [String] -> Matrix Int -> [[String]]
estadosEquivalentes q m =
  let
    listaCoordenadas = [(i, j) | i <- [1..nrows m], j <- [1..ncols m]]
    equivalentes = filter (\(i, j) ->
      j < i && (m ! (i, j) == 0)
      ) listaCoordenadas
  in map (\(i, j) -> [q !! (j-1), q !! (i-1)]) equivalentes
