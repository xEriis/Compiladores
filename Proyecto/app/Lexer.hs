module Lexer (lexerM, prefijoMasLargo) where

import MDD
import Data.Char (isSpace, isAlpha, isDigit)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Function (on)

--------------------------------------------------------------------
-- Función coincideSimb
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
prefijoMasLargo mdd entrada = recorrer (inicialM mdd) Nothing "" entrada
  where
    -- recorrer :: estadoActual -> últimoToken -> lexema -> resto -> resultado
    recorrer :: String -> Maybe (String,String,String) -> String -> String -> Maybe (String,String,String)
    -- Caso base: no hay entrada ni token previo
    recorrer _ Nothing "" [] = Nothing

    -- Caso: llegamos al final de la entrada
    recorrer q ultimoToken lexema []
      | Just tok <- lookup q (finalesM mdd) = Just (tok, lexema, []) -- estado final: token completo
      | otherwise = ultimoToken -- no final: devolvemos lo último válido

    -- Caso general: aún hay caracteres por leer
    recorrer q ultimoToken lexema resto@(c:cs) =
      let
          -- Estados alcanzables desde q con el carácter c
          siguientes = [qn | (q0, sym, qn) <- transicionesM mdd, q0 == q, coincideSimb sym c]

          -- Si el estado actual es final, guardamos el token como posible candidato
          ultimoToken' = case lookup q (finalesM mdd) of
                           Just tok -> Just (tok, lexema, resto)
                           Nothing  -> ultimoToken
      in
          case siguientes of
            [] -> ultimoToken'  -- sin transiciones validas, ent devolver lo último válido
            _  -> 
              -- Hay una o más transiciones válidas, probamos cada una
              let resultados = map (\sig -> recorrer sig ultimoToken' (lexema ++ [c]) cs) siguientes
              in elegirMasLargo resultados ultimoToken' resto

------------------------------------------------------------
-- Función: elegirMasLargo
-- Elige el token con el lexema más largo entre varios posibles.
------------------------------------------------------------
elegirMasLargo :: [Maybe (String,String,String)] -> Maybe (String,String,String) -> String -> Maybe (String,String,String)
elegirMasLargo resultados tokenAnterior _ =
  case catMaybes resultados of -- catMaybes: quita los Nothing y extrae los Just -> [ (String,String,String) ]
    [] -> tokenAnterior -- Si después de filtrar no quedó ninguno, devolvemos el ultimoToken 
    ys -> Just $ maximumBy (compare `on` (\(_,lexema,_) -> length lexema)) ys

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