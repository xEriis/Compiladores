module Main (main) where

import qualified Data.Set as Set
import AFD
import AFN

deltaEj :: Estado -> Simbolo -> EstadosAlcazables
deltaEj 0 'a' = Set.fromList [0,1]
deltaEj 1 'a' = Set.empty
deltaEj _ _   = Set.empty

afnEj :: AFN
afnEj = AFN {
    estadosAFN = [0,1],
    alfabetoAFN = ['a'],
    deltaAFN = deltaEj,
    inicialAFN = 0,
    finalesAFN = [1]
}

main :: IO ()
main = do
    -- Prueba sencilla para AFD
    -- print (acepta "1100" afdPar)
    let afd = trans_afn_a_afd afnEj
    putStrLn "Estados del AFD:"
    print (estadosAFD afd)
    putStrLn "Finales del AFD:"
    print (finalesAFD afd)
