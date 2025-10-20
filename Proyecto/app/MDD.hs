module MDD 
(
    MDD(..),
    TransM,
    buildMDD

) where

import Data.List (nub)
import AFD -- para checar transición

-- Tipo de transición de la MDD: (estado-origen, símbolo, estado-destino)
type TransM = (String, Char, String)

-- Definición de la Máquina Discriminadora Determinista
data MDD = MDD {
    estadosM      :: [String], -- estados de la MDD
    alfabetoM     :: [Char], -- alfabeto reconocido
    transicionesM :: [TransM], -- transiciones (origen, símbolo, destino)
    inicialM      :: String, -- estado inicial
    finalesM      :: [(String, String)] -- (estado final, token asociado)
} deriving (Show)

--------------------------------------------------------
-- Función cambiaNombreAFD
-- Dado un nombre de token y un AFD, renombra
-- todos los estados del AFD con un prefijo para evitar choques
-- de nombres cuando unimos varios AFDs en la MDD.
-- Ejemplo: si el AFD tiene estado "q0" y token="id", el nuevo
-- estado será "id_q0".
--------------------------------------------------------
cambiaNombreAFD :: String -> AFD -> AFD
cambiaNombreAFD token (AFD qs a ts q0 fs) =
    -- Asociamos cada estado con un índice (0,1,2,...) para numerarlos
    let estadosMap = zip qs ([0 :: Int ..])  -- <- evita warning de defaulting

        -- Obtenemos índice de un estado (lanza error si no existe)
        buscaIndice s = case lookup s estadosMap of
                        Just i  -> i
                        Nothing -> error $ "Estado no encontrado en cambiaNombreAFD: " ++ show s

        -- Generamos el nuevo nombre de estado prefijado
        nuevoNombre s = token ++ "_q" ++ show (buscaIndice s)
    in AFD {
      estadosD = map nuevoNombre qs,                                        -- renombramos todos los estados
      alfabetoD = a,                                                            -- conservamos el alfabeto
      transicionesD = [(nuevoNombre q1, c, nuevoNombre q2) | (q1, c, q2) <- ts], -- renombramos tranciones
      inicialD = nuevoNombre q0,                                                   -- renombramos el inicial
      finalesD = map nuevoNombre fs                                                -- renombramos los finales
  }

-----------------------------------
-- Función buildMDD
-- Se encarga de crear la MDD, recibe una lista de pares (nombreToken, AFD)
-- y construye la MDD que pega todos los AFDs.
--
-- Lo que hacemos es:
-- 1) Cambiamos el nombre de los estados de cada AFD para evitar choques de nombres
-- 2) Unimos todos los estados, alfabetos y transiciones
-- 3) Agregamos un estado inicial MDD_0, desde el que añadimos transiciones que correspondan 
--    a las transiciones que salían de cada estado inicial de los AFDs, copiamos símbolo y 
--    estado destino, como si saliera del nuevo estado inicial global: ("MDD_0", símbolo, estado_destino)
-- 4) Los pares (estado_final_cambiadoDeNombre, tokenName) qué token representa cada estado final
-- 5) Eliminamos duplicados de la lista total de transiciones con nub
-----------------------------------
buildMDD :: [(String, AFD)] -> MDD
buildMDD tokens =
    let 
        -- Cambiamos nombres de cada AFD: [(nombreToken, AFD_cambiado)]
        prefijos = [(n, cambiaNombreAFD n a) | (n,a) <- tokens]

        -- Unimos todos los estados de cada AFD
        estados  = concatMap (estadosD . snd) prefijos

        -- Unimos (sin duplicados) de los alfabetos de cada AFD
        alfabeto = nub $ concatMap (alfabetoD . snd) prefijos

        -- Tomamos todas las transiciones internas de los AFDs (con nombre cambiado)
        transInternas = concatMap (transicionesD . snd) prefijos

        -- Creamos transiciones desde MDD_0: toma cada transición que parte del estado inicial del AFD
        -- Para cada AFD: tomamos las transiciones que salgan de su estado inicial y
        -- añadimos una transición desde MDD_0 con el mismo símbolo ( "MDD_0", símbolo, estado_prefijado).
        arranques = [ ("MDD_0", sym, q2)
                        | (_, a) <- prefijos
                        , (q1, sym, q2) <- transicionesD a
                        , q1 == inicialD a
                    ]

        -- Estado inicial de la MDD
        inicial = "MDD_0"

        -- Asignamos los estados finales y su token (estadoNombreCambiado, nombreToken)
        finales = concatMap (\(n, a) -> [(f, n) | f <- finalesD a]) prefijos

        -- Transiciones totales: arranques ++ transInternas, quitando duplicados
        transTotales = nub (arranques ++ transInternas)
    in MDD {
      estadosM = inicial : estados, -- agregamos MDD_0 al principio
      alfabetoM = alfabeto,
      transicionesM = transTotales,
      inicialM = inicial,
      finalesM = finales
    }
