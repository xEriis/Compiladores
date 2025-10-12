module Lexer (lexerM, longestMatchM) where

import MDD
import Data.Char (isSpace, isAlpha, isDigit)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Function (on)
-- import AFD

-- symbolMatches: compara símbolo de transición con carácter de entrada.
-- '#' => cualquier dígito, '@' => cualquier letra, en otro caso coincidencia literal.
symbolMatches :: Char -> Char -> Bool
symbolMatches ts c
  | ts == '#' = isDigit c
  | ts == '@' = isAlpha c
  | otherwise = ts == c

-- Busca el prefijo más largo aceptado por algún token de la MDD
-- Retorna (token, lexema, resto)
-- longestMatchM: ahora lastTok guarda (token, lexema, resto_en_el_momento)
longestMatchM :: MDD -> String -> Maybe (String, String, String)
longestMatchM mdd input = aux (inicialM mdd) Nothing "" input
  where
    -- aux :: estadoActual -> Maybe (token,lexema,resto) -> lexemaEnConstruccion -> restoActual -> resultado
    aux :: String -> Maybe (String,String,String) -> String -> String -> Maybe (String,String,String)
    aux _ Nothing "" [] = Nothing

    -- fin de entrada: si el estado actual es final, devolverlo; si no, devolver el lastTok (ya contiene su resto)
    aux q lastTok bestMatch [] =
      case lookup q (finalesM mdd) of
        Just tok -> Just (tok, bestMatch, [])   -- estado actual es final y rest es vacío
        Nothing  -> lastTok                     -- fallback al último final conocido (si existe)

    -- caso general: hay al menos un caracter
    aux q lastTok bestMatch rest@(c:cs) =
      let -- todas las transiciones posibles desde q con el símbolo c
        nextStates = [qn | (q0, sym, qn) <- transicionesM mdd, q0 == q, symbolMatches sym c]
        -- qn = checaTransicion q c (transicionesM mdd)
        -- nextStates = if qn /= [] then [qn] else []
          -- si el estado actual es final, lo recordamos junto con el lexema y, **muy importante**, el resto en ese momento
        lastTok' = case lookup q (finalesM mdd) of
                      Just tok -> Just (tok, bestMatch, rest)
                      Nothing  -> lastTok
      in case nextStates of
           [] -> lastTok'  -- no hay transiciones: fallback a lastTok' (si existe) o Nothing
           _  -> -- hay una o más ramas: explorar y elegir la mejor (lexema más largo)
                let results = map (\next -> aux next lastTok' (bestMatch ++ [c]) cs) nextStates
                in chooseBest results lastTok' rest

-- chooseBest ahora trabaja con Maybe triples (tok,lexema,resto)
chooseBest :: [Maybe (String,String,String)] -> Maybe (String,String,String) -> String -> Maybe (String,String,String)
chooseBest res lastTokFallback _rest =
  case catMaybes res of
    [] -> lastTokFallback
    ys -> Just $ maximumBy (compare `on` (\(_,lexema,_) -> length lexema)) ys

-- Lexer: repetidamente toma el prefijo más largo y lo devuelve como token
lexerM :: MDD -> String -> [(String, String)]
lexerM _ [] = []
lexerM mdd s@(c:_)
  | isSpace c = lexerM mdd (dropWhile isSpace s)
  | otherwise =
      case longestMatchM mdd s of
        Nothing -> error $ "Token no reconocido cerca de: " ++ take 10 s
        Just (tok, val, rest) -> (tok, val) : lexerM mdd rest
