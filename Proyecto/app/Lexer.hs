module Lexer (lexerM, longestMatchM) where

import MDD
import Data.Char (isSpace)
import Debug.Trace (trace)
import Data.Maybe (catMaybes)
import Data.List (maximumBy)
import Data.Function (on)


-- Busca el prefijo más largo aceptado por algún token de la MDD
-- Retorna (token, lexema, resto)


-- -- longestMatchM con anotación para evitar ambigüedad en show
-- longestMatchM :: MDD -> String -> Maybe (String, String, String)
-- longestMatchM mdd input = aux (inicialM mdd) Nothing "" input
--   where
--     -- lastTok :: Maybe (tokenName, lexemaQueLoProdujo)
--     aux _ Nothing "" [] = Nothing

--     aux q lastTok bestMatch [] =
--       let out :: Maybe (String, String, String)
--           out = case lookup q (finalesM mdd) of
--                   Just tok -> Just (tok, bestMatch, [])
--                   Nothing  -> case lastTok of
--                                 Just (tok', lexema) -> Just (tok', lexema, [])
--                                 Nothing   -> Nothing
--       in trace ("[END] estado: " ++ show q ++
--                 "  lastTok: " ++ show lastTok ++
--                 "  bestMatch: " ++ show bestMatch ++
--                 " => " ++ show (out :: Maybe (String,String,String))) out

--     aux q lastTok bestMatch (c:cs) =
--       let nextStates = [qn | (q0, sym, qn) <- transicionesM mdd, q0 == q, sym == c]
--           lastTok' = case lookup q (finalesM mdd) of
--                        Just tok -> Just (tok, bestMatch)
--                        Nothing  -> lastTok
--           traceMsg = "[STEP] q=" ++ show q ++ "  c=" ++ show c ++
--                      "  bestMatch=" ++ show bestMatch ++ "  nextStates=" ++ show nextStates ++
--                      "  lastTok=" ++ show lastTok'
--       in trace traceMsg $
--          case nextStates of
--            (next:_) -> aux next lastTok' (bestMatch ++ [c]) cs
--            []       -> case lastTok' of
--                          Just (tok, lexema) -> trace ("[FALLBACK] q=" ++ show q ++ "  returning lastTok " ++ show (tok,lexema)) (Just (tok, lexema, c:cs))
--                          Nothing            -> trace ("[NO MATCH] q=" ++ show q ++ "  c=" ++ show c ++ "  no lastTok") Nothing

-- Busca el prefijo más largo aceptado por algún token de la MDD
-- Retorna (token, lexema, resto)
longestMatchM :: MDD -> String -> Maybe (String, String, String)
longestMatchM mdd input = aux (inicialM mdd) Nothing "" input
  where
    -- aux :: estado actual -> Maybe (token, lexemaEncontradoHastaAhora) -> lexemaEnConstruccion -> resto -> resultado
    aux _ Nothing "" [] = Nothing

    aux q lastTok bestMatch [] =
      case lookup q (finalesM mdd) of
        Just tok -> Just (tok, bestMatch, [])
        Nothing  -> case lastTok of
                      Just (tok', lexema) -> Just (tok', lexema, [])
                      Nothing   -> Nothing

    aux q lastTok bestMatch (c:cs) =
      let -- todas las transiciones posibles desde q con el símbolo c
          nextStates = [qn | (q0, sym, qn) <- transicionesM mdd, q0 == q, sym == c]
          -- si el estado actual es final, lo recordamos junto con el lexema actual
          lastTok' = case lookup q (finalesM mdd) of
                       Just tok -> Just (tok, bestMatch)
                       Nothing  -> lastTok
      in case nextStates of
           [] -> -- no hay transiciones: fallback a lastTok (si existe) o nada
                 case lastTok' of
                   Just (tok, lexema) -> Just (tok, lexema, c:cs)
                   Nothing            -> Nothing
           _  -> -- hay varias/una rama(s): probar todas y escoger el mejor (más largo)
                 let results = map (\next -> aux next lastTok' (bestMatch ++ [c]) cs) nextStates
                 in chooseBest results lastTok' (c:cs)

-- Elegir el mejor resultado entre varias ramas (el lexema más largo).
-- Si ninguna rama devuelve Just, usar lastTok' como fallback.
chooseBest :: [Maybe (String,String,String)] -> Maybe (String,String) -> String -> Maybe (String,String,String)
chooseBest res lastTokFallback rest =
  case catMaybes res of
    [] -> case lastTokFallback of
            Just (tok,lexema) -> Just (tok, lexema, rest)
            Nothing           -> Nothing
    ys -> Just $ maximumBy (compare `on` (\(_,lexema,_) -> length lexema)) ys


-- Lexer principal
lexerM :: MDD -> String -> [(String, String)]
lexerM _ [] = []
lexerM mdd s@(c:_)
  | isSpace c = lexerM mdd (dropWhile isSpace s)
  | otherwise =
      case longestMatchM mdd s of
        Nothing -> error $ "Token no reconocido cerca de: " ++ take 10 s
        Just (tok, val, rest) -> (tok, val) : lexerM mdd rest

-- -- Lexer para MDD
-- lexerM :: MDD -> String -> [(String, String)]
-- lexerM mdd input = lexerAux input
--   where
--     trans :: [(String, Char, String)]
--     trans = transicionesM mdd

--     finales :: [(String, String)]
--     finales = finalesM mdd

--     -- Función auxiliar: recorre la cadena
--     lexerAux :: String -> [(String, String)]
--     lexerAux [] = []
--     lexerAux str@(c:cs)
--       | isSpace c = lexerAux cs  -- ignorar espacios
--       | otherwise =
--           case longestMatch "MDD_START" str of
--             Nothing -> error $ "Token no reconocido cerca de: " ++ take 10 str
--             Just (tokenStr, tokenType, rest) ->
--               (tokenType, tokenStr) : lexerAux rest

--     -- Encuentra el token más largo que coincide desde el estado actual
--     longestMatch :: String -> String -> Maybe (String, String, String)
--     longestMatch estado s = go estado s "" Nothing
--       where
--         go _ [] acc lastFinal =
--           case lastFinal of
--             Nothing -> Nothing
--             Just (tok, _) -> Just (acc, tok, [])
--         go q (x:xs) acc lastFinal =
--           let posibles = [q' | (q0, c, q') <- trans, q0 == q, c == x]
--               lastFinal' = case [tok | (qf, tok) <- finales, qf == q] of
--                              [] -> lastFinal
--                              (tok:_) -> Just (tok, acc)
--           in case posibles of
--                [] -> case lastFinal' of
--                        Nothing -> Nothing
--                        Just (tok, tokStr) -> Just (tokStr, tok, x:xs)
--                (q':_) -> go q' xs (acc ++ [x]) lastFinal'

