module AFNe
(
    AFNe(..),
    rmDup,
    afnEp_to_AFN
)
where

import AFN
import Data.Set (toList, fromList)

------------------------------------------------------------
-- Abstracción de una transición epsilón,
-- utilizamos nothing para modelar una transición epsilón.
-- la tercia representa la función delta, el primero es
-- el estado donde se lee el cáracter o el epsilon y te lleva
-- a una lista de estados (no determinista).
------------------------------------------------------------
type Trans_eps = (String, Maybe Char, [String])

-- Automáta no determinista con transiciones epsilon, 
data AFNe = AFNe {estados :: [String], alfabeto :: [Char],
                  transiciones :: [Trans_eps],
                  inicial :: String, final :: String}
  deriving (Show)

----------------------------------------------------
-- Función que manda a llamar las funciones encargadas
-- de la transformación de un autómata AFNe a un
-- AFN.
----------------------------------------------------
afnEp_to_AFN :: AFNe -> AFN
afnEp_to_AFN m =  AFN {estadosN = estados m,
                       alfabetoN =  alfabeto m,
                       transicionesN = trans_eps_to_afn m,
                       inicialN = inicial m, 
                       finalN = final m}

----------------------------------------------------
-- Función la cual define la nueva transición para el
-- AFN resultante, eliminando las transiciones épsilon
-- al no utilizarlas.
----------------------------------------------------
trans_eps_to_afn :: AFNe -> [Trans_afn]
trans_eps_to_afn m = concat $
  map (trans_eps_to_afn_aux m (transiciones m) (alfabeto m)) (estados m)

----------------------------------------------------
-- Función auxiliar para la función de transiciones
-- del AFN, utiliza las funciones closures para seguir
-- las transiciones épsilon para obtener la transición
-- AFN esperada.
----------------------------------------------------
trans_eps_to_afn_aux :: AFNe -> [Trans_eps] -> String -> String -> [Trans_afn]
trans_eps_to_afn_aux _ _ [] _ = []
trans_eps_to_afn_aux m l (c:cs) q = (q, c, qn) : (trans_eps_to_afn_aux m l cs q)
  where qn = eclosure2 m $ do_trans_nep2 l c (eclosure l m q)

----------------------------------------------------
-- Función la cual hace uso de la función de transición épsilon
-- para obtener el cierre épsilon de un estado.
----------------------------------------------------
eclosure ::  [Trans_eps] -> AFNe -> String  -> [String]
eclosure [] _ q1 = [q1]
eclosure ((q2, Nothing, l):xs) m q1
  | q2 == q1 = rmDup $ (q1:l) ++ eclosure2 m l
  | otherwise = eclosure xs m q1
eclosure (_:xs) m q1  = eclosure xs m q1

----------------------------------------------------
-- Función de cierre épsilon para una lista de estados.
----------------------------------------------------
eclosure2 ::  AFNe -> [String]  -> [String]
eclosure2 _ [] = []
eclosure2 m (x:xs) = eclosure (transiciones m) m x ++ eclosure2 m xs

----------------------------------------------------
-- Función que remueve duplicados de la lista de estados.
----------------------------------------------------
rmDup :: (Ord a) => [a] -> [a]
rmDup = toList . fromList

----------------------------------------------------
-- Función la cual regresa la transición de un estado
-- en donde haciendo uso de la transición épsilon y el símbolo
-- obtiene el alcance de esta.
----------------------------------------------------
do_trans_nep :: [Trans_eps] -> Char -> String -> [String]
do_trans_nep [] _ _ = [""]
do_trans_nep ((q1, c1, q2):xs) c2 q3
  | q1 == q3 && (to_char c1) == c2 = q2
  | otherwise            = do_trans_nep xs c2 q3

----------------------------------------------------
-- Función la cual convierte un Maybe Char a Char
-- donde en caso de ser Nothing regresa '~', y en caso
-- de que haya un char regresa ese char.
----------------------------------------------------
to_char :: Maybe Char -> Char
to_char Nothing = '~'
to_char (Just a) = a

----------------------------------------------------
-- Función que hace uso de la función de transición
-- del AFNe para una lista de estados y un símbolo
-- determinado.
----------------------------------------------------
do_trans_nep2 :: [Trans_eps] -> Char -> [String] -> [String]
do_trans_nep2 l c qs = formato $ map (do_trans_nep l c) qs

----------------------------------------------------
-- Función que da formato a la lista de estados.
-- De una lista de listas de estados, devuelve una lista de 
-- strings (estados nuevos).
----------------------------------------------------
formato :: [[String]] -> [String]
formato [] = []
formato (x:xs)
  | x == [""]   = formato xs
  | otherwise =  x++formato xs