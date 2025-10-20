module Regex 
(
  Expr(..),
  regex_to_AFNe,
)
where
  
import AFNe
import Data.List (nub) 
-- nub elimina elementos duplicados de una lista

----------------------------------------
-- Abstracción de una expresión regular
----------------------------------------
data Expr =  
            Term Char          -- símbolo/caracter: 'a'
          | Concat Expr Expr   -- concatenación: ab
          | Or Expr Expr       -- unión: a+b
          | Kleene Expr        -- estrella de kleene: a*

-- Para imprimir las expresiones regulares
instance Show Expr where
  show (Term a)   = a:[]
  show (Concat a b) = "(" ++ show a ++ show b ++ ")"
  show (Or a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Kleene a) = show a ++ "*"

------------------------------------------------------------------
-- Traducción de regex a automátas con transiciones épsilon (AFNe)
------------------------------------------------------------------
regex_to_AFNe :: Expr -> AFNe
regex_to_AFNe e = aux_regex_to_AFNe e []

--------------------------------------------------------
-- Función auxiliar que realiza la construcción del AFNe
-- Expr: regex
-- [String]: lista de nombres de los estados
-- Regresa: AFNe
--------------------------------------------------------
aux_regex_to_AFNe :: Expr -> [String] ->  AFNe

----------------------------------------------------------------------------------------------------------------
-- Caso para Term:
-- Construimos el autómata para un símbolo: creamos dos estados q0 -> q1 con la transición con el símbolo 'a'
----------------------------------------------------------------------------------------------------------------
aux_regex_to_AFNe (Term a) q = AFNe {estados = q ++ [q0, q1],
                                    alfabeto = [a],
                                    transiciones = [(q0, Just a, [q1])], -- transición etiquetada con el símbolo
                                    inicial = q0, 
                                    final = q1}
  -- nombres de los dos nuevos estados basados en la longitud de "q"
  where q0 = "q" ++ show (length q)
        q1 = "q" ++ show ((length q) + 1)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Caso para Or:
-- Construimos el autómata para la unión: creamos dos nuevos estados, uno inicial q0 y uno final q1. Conectamos con transiciones-ε de q0 a los iniciales de m1 y m2
-- y los finales de m1, m2 se conectan con transiciones-ε a q1.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
aux_regex_to_AFNe (Or a b) q =  AFNe {estados = (estados m2) ++ [q0, q1],
                                      alfabeto = nub ((alfabeto m1) ++ (alfabeto m2)), -- unimos los alfabetos y utilizamos nub para evitar duplicados en el alfabeto
                                      transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(q0, Nothing, [(inicial m1), (inicial m2)]),
                                         (final m1, Nothing, [q1]),
                                         (final m2, Nothing, [q1])],
                                      inicial = q0, 
                                      final = q1}
  where m1 = aux_regex_to_AFNe a q
        m2 = aux_regex_to_AFNe b (q ++ (estados m1))
        -- nombres de los dos nuevos estados basados en la longitud de "q"
        q0 = "q" ++ show (length $ estados m2)
        q1 = "q" ++ show ((length $ estados m2) + 1)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Caso para Concat:
-- Construimos el autómata para la concatenación: conectamos con transiciones-ε el estado final (o finales) de m1 con el inicial de m2 y ahora el estado inicial será el inicial de m1 y 
-- el final será el final de m2.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
aux_regex_to_AFNe (Concat a b) q = AFNe {estados = estados m2,
                                    alfabeto = nub ((alfabeto m1) ++ (alfabeto m2)), -- nub para evitar duplicados en el alfabeto
                                    transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(final m1, Nothing, [inicial m2])],
                                    inicial = inicial m1, 
                                    final = final m2}
  where m1 = aux_regex_to_AFNe a q
        m2 = aux_regex_to_AFNe b (q ++ (estados m1))
  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Caso para Kleene:
-- Construimos el autómata para la estrella de kleene: creamos dos nuevos estados, uno inicial q0 y uno final q1, los conectamos con una transición-ε (q0->q1) 
-- para permitir 0 repeticiones y para más repeticiones usamos transiciones-ε (qf -> qi), además de que conectamos con una transición-ε de q0 al inicial qi
-- y el final qf se conecta con una transición-ε a q1.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
aux_regex_to_AFNe (Kleene a) q =  AFNe {estados = (estados m1) ++ [q0, q1],
                                    alfabeto = (alfabeto m1),
                                    transiciones = (transiciones m1)
                                      ++ [(q0, Nothing, [qi, q1]),
                                         (qf, Nothing, [qi, q1])],
                                    inicial = q0, 
                                    final = q1}
  where m1 = aux_regex_to_AFNe a q
        -- nombres de los dos nuevos estados basados en la longitud de "q"
        q0 = "q" ++ show (length $ estados m1)
        q1 = "q" ++ show ((length $ estados m1) + 1)
        qi = inicial m1
        qf = final m1 

