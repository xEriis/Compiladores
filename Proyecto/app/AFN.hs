module AFN
(
    AFN(..),
    Trans_afn
)
where

-- Elementos del autómata finito no determinista
-- BIBLIOTECAS (están comentadas para que no generen warning)
--import Data.List (nub,sort)
--import AFD

-- | Abstracción de una transición no determinista, 
-- igual que la anterior, sin considerar epsilón, pero
-- sigue siendo no determinista. 
type Trans_afn = (String, Char, [String])
data AFN = AFN {
    estadosN :: [String], 
    alfabetoN :: [Char],
    transicionesN :: [Trans_afn],
    inicialN :: String, finalN :: String}
  deriving (Show)

-- TODO Conversion. MM aquí tuve problema porque el data definido en AFD no crresponde con el de aquí.
-- Así daba error al convertir uno en otro.
-- por eso comenté todo jajaj




{-type Estado = Int
type Simbolo = Char
type EstadosAlcazables = [Estado] -- Conjunto de estados alcanzables a partir de un estado

type DeltaND = Estado -> Simbolo -> [Estado] --la función de transición ahora nos puede llevar a una lista de estados
data AFN1 = AFN1 {
    estadosAFN :: [Estado],
    alfabetoAFN :: [Simbolo], --Aunque el alfabeto será el mismo que el AFD
    deltaAFN :: DeltaND,
    inicialAFN :: Estado,
    finalesAFN :: [Estado]
}
nuevosEstadosFinales :: AFN1 -> [Estado] -> Bool -- Si un estado final está en alguno de los subconjuntos
nuevosEstadosFinales afn estados = any (`elem` finalesAFN afn) estados

-- Partimos de unir los estados alcanzables para crear nuevos (estos funcionan como nuestros nuevos estados)
subconjuntosAFN :: AFN1 -> [Estado] -> Simbolo -> [Estado]
-- nub elimina duplicados, mientras que sort como se intuye, ordena. (En haskell [0,1] es distinto de [1,0], tiene sentido, por lo que al ordenarlos solo tenemos una opción)
-- La unica mala "desición" que tenemos esque nub es O(n)^2, por lo cual si el profe nos pide optimizar conviene mejor set o monedas
subconjuntosAFN afn estados simbolo = nub . sort . concat $ [deltaAFN afn e simbolo | e <- estados]

{-|
Función la cual hace la conversión del autómata finito no determinista a un autómata finito determinista
recibe un AFN y lo transforma en un AFD, cada conjunto de estados representa un estado.
-}
trans_afn_a_afd :: AFN1 -> AFD
trans_afn_a_afd afn = 
    let estadoinicial = [inicialAFN afn] --iniciamos del estado inicial del autómata finito no determinista
        -- Los nuevos que iremos obteniendo son aquellos que nos llevan de uno o más estados alcanzables hacia más estados alcazables
        estadosNuevos :: [[Estado]] -> [[Estado]] -> [[Estado]]
        estadosNuevos revisados [] = revisados
        estadosNuevos revisados (actual:siguiente) = 
            -- los estados se procesan estando en el actual con el símbolo correspondiente en el afn
            let estados = [subconjuntosAFN afn actual simbolo | simbolo <- alfabetoAFN afn]
                -- Eliminación de repetidos y ordenamiento para evitar listas distintas
                ordenamientoEstados = filter (not . null) (map (nub . sort) estados)
                -- estados no revisados asegurados que no están repetidos y no nulos
                noRevisado = filter (`notElem` (revisados ++ siguiente)) ordenamientoEstados
                -- Listas de los estados que ya revisamos en el AFN y de aquellos que no.
                estadosRecorridos = (revisados ++ [actual]) 
                noRecorridos = siguiente ++ noRevisado
            in estadosNuevos estadosRecorridos noRecorridos

        -- Partes resultantes de la autómata finita determinista
        delta estados simbolo = subconjuntosAFN afn estados simbolo 
        estadosNuevosAFD = estadosNuevos [] [estadoinicial]
        estadosFinales = filter(nuevosEstadosFinales afn) estadosNuevosAFD

    in AFD{
        estados= concat estadosNuevosAFD,
        alfabeto= alfabetoAFN afn, --mismo por ser equivalente
        delta = delta,
        inicial = concat estadoinicial,
        finales = concat estadosFinales
    }


data AFD = AFD {
    estadosAFD :: [[Estado]], --Lista de estados
    alfabetoAFD :: [Simbolo], --Simbolos 
    deltaAFD :: [Estado] -> Simbolo -> [Estado],
    inicialAFD :: [Estado], 
    finalesAFD :: [[Estado]]
}
-}