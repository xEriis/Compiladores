module AFN

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

{-|
Función la cual hace la conversión del autómata finito no determinista a un autómata finito determinista
recibe un AFN y lo transforma en un AFD, cada conjunto de estados representa un estado.
-}
trans_afn_a_afd :: AFN -> AFD
trans_afn_a_afd afn = 
    let estadoinicial = fromList [inicialAFN afn] --iniciamos del estado inicial del autómata finito no determinista
        -- Los nuevos que iremos obteniendo son aquellos que nos llevan de uno o más estados alcanzables hacia más estados alcazables
        estadosNuevos :: [EstadosAlcazables] -> [EstadosAlcazables] -> [EstadosAlcazables]
        estadosNuevos revisados [] = revisados
        estadosNuevos revisados (actual:siguiente) = 
            -- los estados se procesan estando en el actual con el símbolo correspondiente en el afn
            let estados = [subconjuntosAFN afn actual simbolo | simbolo <- alfabetoAFN afn]
                -- estados no revisados con lambda, si su elemento no está en revisado y si 
                noRevisado = filter (\nr -> not (nr `elem` revisados) && not (null nr)) estados
                -- Listas de los estados que ya revisamos en el AFN y de aquellos que no.
                estadosRecorridos = revisados ++ [actual] 
                noRecorridos = siguiente ++ noRevisado
            in estadosNuevos estadosRecorridos noRecorridos

        -- Partes resultantes de la autómata finita determinista
        --función de transición delta (sin normalizar, vamos de un conjunto de estados a mutiples conjuntos de estados)
        delta :: EstadosAlcazables -> Simbolo -> EstadosAlcazables
        delta cjtoEstados simbolo = subconjuntosAFN afn cjtoEstados simbolo
        estadosNuevosAFD = estadosNuevos [] [estadoinicial]
        estadosFinales = filter(nuevosEstadosFinales afn) estadosNuevosAFD

    in AFD{
        estadosAFD = estadosNuevosAFD,
        alfabetoAFD = alfabetoAFN afn, --mismo por ser equivalente
        deltaAFD = delta,
        inicialAFD = estadoinicial,
        finalesAFD = estadosFinales
    }
