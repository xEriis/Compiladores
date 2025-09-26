module AFNe

where

type Estado = Int
type Simbolo = Char
type EstadosAlcazables = [Estado]
epsilon :: Simbolo
-- Podriamos cambiarlo a "Epsilon", pero los simbolos son chars, si Simbolo fuera string se podría
-- pero eso consumiría aún más memoria
epsilon = 'ε' 

type DeltaE = Estado -> Simbolo -> [Estado] --la función de transición ahora nos puede llevar a una lista de estados
data AFNe = AFNe {
    estadosAFNe :: [Estado],
    alfabetoAFNe :: [Simbolo], --Aunque el alfabeto será el mismo que el AFD
    deltaAFNe :: DeltaE,
    inicialAFNe :: Estado,
    finalesAFNe :: [Estado]
}

type DeltaND = Estado -> Simbolo -> [Estado] --la función de transición nos sigue llevando de un estado a una lista de estados
data AFN = AFN {
    estadosAFN :: [Estado],
    alfabetoAFN :: [Simbolo], --Aunque el alfabeto será el mismo que el AFD
    deltaAFN :: DeltaND,
    inicialAFN :: Estado,
    finalesAFN :: [Estado]
}

-- Función de los estados alcanzables con épsilon
alcanzaEpsilon :: AFNe -> Estado -> [Estado]
alcanzaEpsilon afne estado  = cierre [estado] [estado] -- cierre o unión de las epsilon transiciones
    where
        cierre recorrido [] = recorrido
        cierre recorrido (actual: siguiente) = 
            let
                estadoCerrado = filter (`notElem` recorrido) (deltaAFNe afne actual epsilon)    
            in cierre (recorrido ++ estadoCerrado) (siguiente ++ estadoCerrado)

-- Función para eliminar las épsilon transiciones del autómata
eliminaEpsilon :: AFNe -> Estado -> Simbolo -> [Estado]
eliminaEpsilon afne estado simbolo = 
    let
        estadoNuevo = alcanzaEpsilon afne estado
        alcanzablesEpsilon = concatMap (\e -> deltaAFNe afne e simbolo) estadoNuevo
        alcanzablesAFN = concatMap (alcanzaEpsilon afne) alcanzablesEpsilon
    in alcanzablesAFN

estadosFinales :: AFNe -> [Estado]
estadosFinales afne = [estado | estado <- estadosAFNe afne, any (`elem` finalesAFNe afne) (alcanzaEpsilon afne estado)]

afne_a_afn :: AFNe -> AFN
afne_a_afn afne = 
    AFN {
        estadosAFN = estadosAFNe afne,
        alfabetoAFN = filter (/= epsilon) (alfabetoAFNe afne),
        deltaAFN = eliminaEpsilon afne,
        inicialAFN = inicialAFNe afne,
        finalesAFN = estadosFinales afne
    }

