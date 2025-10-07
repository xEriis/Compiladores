module MDD 
(
    MDD(..),
    TransM,
    buildMDD,
    transitaM,
    aceptaM
) where

import Data.List (nub)
import AFD

-- Tipo de transición de la MDD
type TransM = (String, Char, String)

-- Definición de la Máquina Discriminadora Determinista
data MDD = MDD {
    estadosM      :: [String],
    alfabetoM     :: [Char],
    transicionesM :: [TransM],
    inicialM      :: String,
    finalesM      :: [(String, String)]
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
      -- añadimos una transición desde "MDD_START" con la misma etiqueta.
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



-- Simulación simple (devuelve último estado alcanzado)
transitaM :: String -> String -> [TransM] -> String
transitaM q [] _ = q
transitaM q (c:cs) d =
  case [qn | (q0, sym, qn) <- d, q0 == q, sym == c] of
    (next:_) -> transitaM next cs d
    []       -> q

-- aceptaM: devuelve el token del último estado final alcanzado mientras recorre la cadena
aceptaM :: String -> MDD -> Maybe String
aceptaM s mdd@(MDD _ _ _ q0 f) = aux q0 s Nothing
  where
    aux q [] lastFinal =
      case lookup q f of
        Just tok -> Just tok
        Nothing  -> lastFinal
    aux q (c:cs) lastFinal =
      let nextStates = [qn | (q0', sym, qn) <- transicionesM mdd, q0' == q, sym == c]
          lastFinal' = case lookup q f of
                         Just tok -> Just tok
                         Nothing  -> lastFinal
      in case nextStates of
           (next:_) -> aux next cs lastFinal'
           []       -> lastFinal'
