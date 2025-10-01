module Regex 
(
  Expr(..),
  expr_to_AFNEp,
)
where
  
import AFNe
import Data.List (nub)

-- Abstracción de una expresión regular.
data Expr =  Term Char | And Expr Expr | Or Expr Expr | Kleene Expr

instance Show Expr where
  show (Term c)   = c:[]
  show (And  l r) = "("++show l ++ show r ++ ")"
  show (Or   l r) = "("++show l ++ "+" ++ show r ++ ")"
  show (Kleene e) = show e ++ "*"

-- | Traducción de expresiones regulares a automátas con transiciones epsilón
expr_to_AFNEp :: Expr -> AFNEp
expr_to_AFNEp e = expr_to_AFNEp_aux e []

expr_to_AFNEp_aux :: Expr -> [String] ->  AFNEp
expr_to_AFNEp_aux (Term a) q = AFNEp {estados = q++[q0, q1],
                                    alfabeto =  [a],
                                    transiciones = [(q0, Just a, [q1])],
                                    inicial = q0, final = q1}
  where q0 = "q"++(show (length q))
        q1 = "q"++(show ((length q) + 1))
expr_to_AFNEp_aux (Or a b) q =  AFNEp {estados = (estados m2) ++ [q0, q1],
                                    alfabeto =  rmDup $  nub ((alfabeto m1) ++ (alfabeto m2)),
                                    transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(q0, Nothing, [(inicial m1),(inicial m2)]),
                                         (final m1, Nothing, [q1]),
                                         (final m2, Nothing, [q1])],
                                    inicial = q0, final = q1}
  where m1 = expr_to_AFNEp_aux a q
        m2 = expr_to_AFNEp_aux b (q++(estados m1))
        q0 = "q"++(show  (length $ estados m2))
        q1 = "q"++(show  ((length $ estados m2) + 1))
expr_to_AFNEp_aux (And a b) q =  AFNEp {estados = estados m2,
                                    alfabeto =  nub ((alfabeto m1) ++ (alfabeto m2)),
                                    transiciones = (transiciones m1) ++ (transiciones m2)
                                      ++ [(final m1, Nothing, [inicial m2])],
                                    inicial = inicial m1, final = final m2}
  where m1 = expr_to_AFNEp_aux a q
        m2 = expr_to_AFNEp_aux b (q++(estados m1))
expr_to_AFNEp_aux (Kleene a) q =  AFNEp {estados = (estados m1) ++ [q0, q1],
                                    alfabeto =  (alfabeto m1),
                                    transiciones = (transiciones m1)
                                      ++ [(q0, Nothing, [qi, q1]),
                                         (qf, Nothing, [qi, q1])],
                                    inicial = q0, final = q1}
  where m1 = expr_to_AFNEp_aux a q
        q0 = "q"++(show  (length $ estados m1))
        q1 = "q"++(show  ((length $ estados m1) + 1))
        qi = inicial m1
        qf = final m1 

