module AFD
(
    AFD(..),
    transita,
    acepta,
    deltaPar,
    afdPar
)
where
-- Elementos del autómata finito determinista
type Estado = Int
type Simbolo = Char
type Delta = Estado -> Simbolo -> Estado
-- Quintupla del estado finito determinista
data AFD = AFD {
    estados :: [Estado],
    alfabeto :: [Simbolo],
    delta :: Delta,
    inicial :: Estado,
    finales :: [Estado]
}

transita :: String -> Estado -> Delta -> Estado
transita [] q d = q
transita (s:cs) q d = transita cs q' d
   where q' = d q s

acepta :: String -> AFD -> Bool
acepta s (AFD q e d q0 f) = elem(transita s q0 d)f


-- Ejemplo de una autómata
deltaPar :: Delta
deltaPar 0 '0' = 0
deltaPar 0 '1' = 1
deltaPar 1 '0' = 1
deltaPar 1 '1' = 0
deltaPar _  _  = error "Símbolo inválido"
-- Automata
afdPar :: AFD
afdPar = AFD { 
    estados  = [0,1],
    alfabeto = ['0','1'],
    delta    = deltaPar,
    inicial  = 0,
    finales  = [0]
}

--Añadir función minimizadora?