module Lexer (lexerM, longestMatchM) where

import MDD
import Data.Char (isSpace, isAlpha, isDigit)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Function (on)

-- symbolMatches: compara símbolo de transición con carácter de entrada.
-- '#' => cualquier dígito, '@' => cualquier letra, en otro caso coincidencia literal.
symbolMatches :: Char -> Char -> Bool
symbolMatches ts c
  | ts == '#' = isDigit c
  | ts == '@' = isAlpha c
  | otherwise = ts == c

-- Busca el prefijo más largo aceptado por algún token de la MDD
-- Retorna (token, lexema, resto)
longestMatchM :: MDD -> String -> Maybe (String, String, String)
longestMatchM mdd input = aux (inicialM mdd) Nothing "" input
  where
    -- aux estado actual -> último token final encontrado (token,lexema) -> lexema en construcción -> resto -> resultado
    aux :: String -> Maybe (String, String) -> String -> String -> Maybe (String, String, String)
    aux _ Nothing "" [] = Nothing

    aux q lastTok bestMatch [] =
      case lookup q (finalesM mdd) of
        Just tok -> Just (tok, bestMatch, [])
        Nothing  -> case lastTok of
                      Just (tok', lexema) -> Just (tok', lexema, [])
                      Nothing             -> Nothing

    aux q lastTok bestMatch (c:cs) =
      let nextStates = [ qn | (q0, sym, qn) <- transicionesM mdd, q0 == q, symbolMatches sym c ]
          lastTok'   = case lookup q (finalesM mdd) of
                         Just tok -> Just (tok, bestMatch)
                         Nothing  -> lastTok
      in case nextStates of
           [] -> case lastTok' of
                   Just (tok, lexema) -> Just (tok, lexema, c:cs)
                   Nothing            -> Nothing
           _  -> -- si hay varias ramas (debería ser raro en un AFD limpio), probamos todas y elegimos la mejor
                 let results = map (\next -> aux next lastTok' (bestMatch ++ [c]) cs) nextStates
                 in chooseBest results lastTok' (c:cs)

-- Elige el resultado con lexema más largo; si ninguna rama devuelve Just, usa el lastTokFallback.
chooseBest :: [Maybe (String,String,String)] -> Maybe (String,String) -> String -> Maybe (String,String,String)
chooseBest res lastTokFallback rest =
  case catMaybes res of
    [] -> case lastTokFallback of
            Just (tok,lexema) -> Just (tok, lexema, rest)
            Nothing           -> Nothing
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
