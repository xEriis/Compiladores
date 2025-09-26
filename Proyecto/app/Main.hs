module Main (main) where
import AFNe

-- Prueba de AFN a AFD
-- deltaEj :: Estado -> Simbolo -> [Estado]
-- deltaEj 0 'a' = [0,1]
-- deltaEj 1 'a' = []
-- deltaEj _ _   = [] -- Cualquier otro caso, vacío

-- afnEj :: AFN
-- afnEj = AFN {
--     estadosAFN = [0,1],
--     alfabetoAFN = ['a'],
--     deltaAFN = deltaEj,
--     inicialAFN = 0,
--     finalesAFN = [1]
-- }

ejemploAFNe :: AFNe
ejemploAFNe = AFNe {
    estadosAFNe = [0,1,2],
    alfabetoAFNe = ['a','b',epsilon],
    deltaAFNe = delta,
    inicialAFNe = 0,
    finalesAFNe = [2]
}
  where
    delta 0 'ε' = [1]
    delta 1 'a' = [1]
    delta 1 'b' = [2]
    delta _  _  = []

main :: IO ()
main = do
    -- Prueba sencilla para AFD
    -- print (acepta "1100" afdPar) --booleano para saber si es una cadena es aceptada
    -- let afd = trans_afn_a_afd afnEj
    -- putStrLn "Estados del AFD:"
    -- print (estadosAFD afd)
    -- putStrLn "Finales del AFD:"
    -- print (finalesAFD afd)

    let afn = afne_a_afn ejemploAFNe
    putStrLn "Estados del AFN:"
    print (estadosAFN afn)
    putStrLn "Alfabeto del AFN:"
    print (alfabetoAFN afn)
    putStrLn "Estado inicial del AFN:"
    print (inicialAFN afn)
    putStrLn "Estados finales del AFN:"
    print (finalesAFN afn)
    putStrLn "Transiciones del AFN:"
    putStrLn "(0,'a') ="
    print (deltaAFN afn 0 'a')
    putStrLn "(0,'b') ="
    print (deltaAFN afn 0 'b')
    putStrLn "(1,'a') ="
    print (deltaAFN afn 1 'a')
    putStrLn "(1,'b') ="
    print (deltaAFN afn 1 'b')

