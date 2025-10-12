module MDD 
(
    MDD(..),
    TransM,
    buildMDD

) where

import Data.List (nub)
import AFD -- para checar transicion
import Data.Char (isAlpha, isDigit)

-- Tipo de transición de la MDD
type TransM = (String, Char, String)

-- Definición de la Máquina Discriminadora Determinista
data MDD = MDD {
    estadosM      :: [String], -- todos los estados de la MDD
    alfabetoM     :: [Char], -- alfabeto reconocido
    transicionesM :: [TransM], -- transiciones (origen, símbolo, destino)
    inicialM      :: String, -- estado inicial
    finalesM      :: [(String, String)] -- (estado final, token asociado)
} deriving (Show)

-- Prefija (normaliza) los nombres de los estados de un AFD
prefijaAFD :: String -> AFD -> AFD
prefijaAFD tok (AFD qs a ts q0 fs) =
  let estadosMap = zip qs ([0 :: Int ..])  -- <- evita warning de defaulting
      lookupIndex s = case lookup s estadosMap of
                        Just i  -> i
                        Nothing -> error $ "Estado no encontrado en prefijaAFD: " ++ show s
      pre s = tok ++ "_q" ++ show (lookupIndex s)
  in AFD {
      estadosD = map pre qs,
      alfabetoD = a,
      transicionesD = [(pre q1, c, pre q2) | (q1, c, q2) <- ts],
      inicialD = pre q0,
      finalesD = map pre fs
  }

buildMDD :: [(String, AFD)] -> MDD
buildMDD toks =
  let prefijos = [(n, prefijaAFD n a) | (n,a) <- toks]
      estados  = concatMap (estadosD . snd) prefijos
      alfabeto = nub $ concatMap (alfabetoD . snd) prefijos

      -- 1) todas las transiciones internas (ya prefijadas)
      transInternas = concatMap (transicionesD . snd) prefijos

      -- 2) transiciones desde MDD_0: toma cada transición que parte del estado inicial del AFD
      -- Para cada AFD: por cada transición que salga del estado inicial,
      -- añadimos una transición desde "MDD_0" con la misma etiqueta.
      arranques = [ ("MDD_0", sym, q2)
                    | (_, a) <- prefijos
                    , (q1, sym, q2) <- transicionesD a
                    , q1 == inicialD a
                  ]

      inicial = "MDD_0"

      -- 3) finales: (estadoPrefijado, tokenName)
      finales = concatMap (\(n, a) -> [(f, n) | f <- finalesD a]) prefijos

      transTotales = nub (arranques ++ transInternas)
  in MDD {
      estadosM = inicial : estados,
      alfabetoM = alfabeto,
      transicionesM = transTotales,
      inicialM = inicial,
      finalesM = finales
    }
