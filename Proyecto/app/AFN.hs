module AFN
(
    AFN(..)
)
where
-- Buscando en internet, set en haskell permite operaciones con conjuntos
-- (ya que el método de AFN a AFD es con operaciones de conjuntos)
import qualified Data.Set as Set
import Data.Set (Set, fromList, toList, unions, map) --Set es quien vamos a usar 

-- Elementos del autómata finito no determinista
type Estado = Int
type Simbolo = Char
type EstadosAlcazables = Set Estado -- Conjunto de estados alcanzables a partir de un estado

type DeltaND = Estado -> Simbolo -> EstadosAlcazables --la función de transición ahora nos puede llevar a una lista de estados
data AFN = AFN {
    estadosAFN :: [Estado],
    alfabetoAFN :: [Simbolo], --Aunque el alfabeto será el mismo que el AFD
    deltaAFN :: DeltaND,
    inicialAFN :: Estado,
    finalesAFN :: [Estado]
}


-- Quintupla del estado finito determinista
-- Usaremos los conjuntos en crudo para determinar a donde se va, es decir, el conjunto de estados alcanzables
-- se volverá un estado nuevo posteriormente en el AFN
type DeltaD = EstadosAlcazables -> Simbolo -> EstadosAlcazables --Función de transición de autómata determinista
data AFD = AFD {
    estadosAFD :: [EstadosAlcazables], --Lista de estados
    alfabetoAFD :: [Simbolo], --Simbolos 
    deltaAFD :: DeltaD, --Ahora vamos de conjuntos de estados con un simbolo a otro conjunto de estados
    inicialAFD :: EstadosAlcazables, 
    finalesAFD :: [EstadosAlcazables]
}

nuevosEstadosFinales :: AFN -> EstadosAlcazables -> Bool -- Si un estado final está en alguno de los subconjuntos
nuevosEstadosFinales afn estados = any (`elem` finalesAFN afn) (toList estados)

-- Partimos de unir los estados alcanzables para crear nuevos (estos funcionan como nuestros nuevos estados)
subconjuntosAFN :: AFN -> EstadosAlcazables -> Simbolo -> EstadosAlcazables
subconjuntosAFN afn estados simbolo = unions [deltaAFN afn estado simbolo | estado <- toList estados]

-- trans_afn_a_afd :: AFN -> AFD

