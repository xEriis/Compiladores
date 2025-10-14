module Lexer (lexerM, prefijoMasLargo) where

import MDD
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (catMaybes)

--------------------------------------------------------------------
-- Función coincideSim
-- Compara símbolo de transición con un carácter de entrada.
-- '#' representa cualquier dígito, 
-- '@' representa cualquier letra, 
-- en otro caso cualquier otro carácter debe coincidir literalmente
---------------------------------------------------------------------
coincideSimb :: Char -> Char -> Bool
coincideSimb ts c
  | ts == '#' = isDigit c
  | ts == '@' = isAlpha c
  | otherwise = ts == c

---------------------------------------------------------------
-- Función prefijoMasLargo
-- Dada una MDD y una cadena de entrada, encuentra el prefijo 
-- más largo aceptado por algún token.
-- Regresa: 
-- * Nothing si no hay coincidencia
-- * Just (token, lexemaReconocido, restoCadena)
----------------------------------------------------------------
prefijoMasLargo :: MDD -> String -> Maybe (String, String, String)
prefijoMasLargo mdd input = aux mdd (inicialM mdd) Nothing "" input


-----------------------------------------------------
-- Función auxiliar
-- Realiza la simulación de la MDD carácter por carácter
-- para encontrar el token más largo posible.
--
-- Parámetros:
--   * mdd: máquina discriminadora determinista
--   * q: estado actual
--   * lastTok: último token final reconocido (Maybe)
--   * lexema: lexema en construcción
--   * resto: parte de la cadena que falta analizar
-----------------------------------------------------
-- aux :: MDD -> EstadoActual -> ÚltimoToken -> Lexema -> Resto -> Resultado
aux :: MDD -> String -> Maybe (String,String,String) -> String -> String -> Maybe (String,String,String)
-- Caso base: no hay entrada ni token previo
aux _ _ Nothing "" [] = Nothing

-- Caso: llegamos al final de la entrada
aux mdd q lastTok lexema [] =
  case lookup q (finalesM mdd) of
    Just tok -> Just (tok, lexema, [])   -- estado final: token completo
    Nothing  -> lastTok                   -- no final: devolvemos lo último válido

-- Caso general: aún hay caracteres por leer
aux mdd q lastTok lexema rest@(c:cs) =
  let 
    -- Estados alcanzables desde q con el carácter c
    siguientes = [q2 | (q1, sym, q2) <- transicionesM mdd, q1 == q, coincideSimb sym c]

    -- Si el estado actual es final, guardamos el token como posible candidato
    lastTok' = case lookup q (finalesM mdd) of
                  Just tok -> Just (tok, lexema, rest)
                  Nothing  -> lastTok
  in case siguientes of
      [] -> lastTok'  -- sin transición válida, ent regresamos al último token conocido
      _  ->
        -- Hay una o más transiciones válidas, probamos cada una
        let resultados = map (\q2 -> aux mdd q2 lastTok' (lexema ++ [c]) cs) siguientes
        in elegirMejor resultados lastTok' rest

----------------------------------------------------
-- Función elegirMejor
-- Recibe una lista de resultados posibles y el último token válido
-- Devuelve el token con lexema más largo o, si no hay ninguno,
-- el último token conocido.
-----------------------------------------------------
elegirMejor :: [Maybe (String,String,String)] -> Maybe (String,String,String) -> String -> Maybe (String,String,String)
elegirMejor resultados ultimoToken _ =
  case catMaybes resultados of    -- catMaybes: quita los Nothing y extrae los Just -> [ (String,String,String) ]
    [] -> ultimoToken            -- Si después de filtrar no quedó ninguno, devolvemos el ultimoToken  
    ys -> Just (mejorPorLongitud ys)

--------------------------------------------------------------------------
-- Función auxiliar mejorPorLongitud
-- Selecciona la tupla con el lexema más largo de una lista de candidatos.
-- Cada tupla tiene la forma (token, lexema, restoCadena).
--  Si hay varios con la misma longitud, se queda con el primero encontrado.
---------------------------------------------------------------------------
mejorPorLongitud :: [(String,String,String)] -> (String,String,String)
mejorPorLongitud = foldl1 (\a@(_,lexA,_) b@(_,lexB,_) -> if length lexA >= length lexB then a else b)

----------------------------------------------------------------
--- Función lexerM
-- Es la función principal del lexer: recorre toda la entrada,
-- detectando token por token hasta aacabar la cadena.
---------------------------------------------------------------
lexerM :: MDD -> String -> [(String, String)]
lexerM _ [] = []    -- Si la entrada está vacía, no hay tokens: devolvemos lista vacía
lexerM mdd s@(c:_)
  | isSpace c = lexerM mdd (dropWhile isSpace s) -- ignoramos espacios o tab
  | otherwise =
      case prefijoMasLargo mdd s of
        -- Si no encontramos ningún prefijo válido, lanzamos error (lexer falla)
        -- take 10 s devuelve solo los primeros 10 caracteres de la cadena s (para manejar error)
        Nothing -> error $ "Token no reconocido: " ++ take 10 s 
        -- Si hay coincidencia: devolvemos el par (token, lexema) y llamamos recursivamente con el resto de la cadena
        Just (tok, val, rest) -> (tok, val) : lexerM mdd rest
