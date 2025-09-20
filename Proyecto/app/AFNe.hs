module AFNe where
    
import Data.Set (toList, fromList)

-- Abstracción de una expresión regular.
data Expr =  Term Char | And Expr Expr | Or Expr Expr | Kleene Expr

instance Show Expr where
  show (Term c)   = c:[]
  show (And  l r) = "("++show l ++ show r ++ ")"
  show (Or   l r) = "("++show l ++ "+" ++ show r ++ ")"
  show (Kleene e) = show e ++ "*"

-- | Abstracción de una transición epsilón,
-- utilizamos nothing para modelar una transición epsilón.
-- la tercia representa la función delta, el primero es
-- el estado donde se lee el cáracter o el epsilon y te lleva
-- a una lista de estados (no determinista).
type Trans_eps = (String, Maybe Char, [String])

-- Automáta no determinista con transiciones epsilon, 
data AFNEp = AFNEp {estados :: [String], alfabeto :: [Char],
                  transiciones :: [Trans_eps],
                  inicial :: String, final :: String}
  deriving (Show)

-- | Abstracción de una transición no determinista, 
-- igual que la anterior, sin considerar epsilón, pero
-- sigue siendo no determinista. 
type Trans_afn = (String, Char, [String])
data AFN = AFN {estadosN :: [String], alfabetoN :: [Char],
                  transicionesN :: [Trans_afn],
                  inicialN :: String, finalN :: String}
  deriving (Show)

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
                                    alfabeto =  rmDup $  (alfabeto m1) ++ (alfabeto m2),
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
                                    alfabeto =  (alfabeto m1) ++ (alfabeto m2),
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

e1 = And (Kleene (Or (Term '0') (Term '1'))) (Term '1')

m1 = AFNEp {estados = ["q0","q1","q2","q3","q4","q5","q6","q7","q8","q9"],
             alfabeto = "01",
             transiciones = [("q0",Just '0',["q1"]),
                             ("q2",Just '1',["q3"]),
                             ("q4",Nothing,["q0","q2"]),
                             ("q1",Nothing,["q5"]),
                             ("q3",Nothing,["q5"]),
                             ("q6",Nothing,["q4","q7"]),
                             ("q5",Nothing,["q4","q7"]),
                             ("q8",Just '1',["q9"]),
                             ("q7",Nothing,["q8"])],
              inicial = "q6", final = "q9"}
    
afnEp_to_AFN :: AFNEp -> AFN
afnEp_to_AFN m =  AFN {estadosN = estados m,
                       alfabetoN =  alfabeto m,
                       transicionesN = trans_eps_to_afn m,
                       inicialN = inicial m, finalN = final m}
               
trans_eps_to_afn :: AFNEp -> [Trans_afn]
trans_eps_to_afn m = concat $
  map (trans_eps_to_afn_aux m (transiciones m) (alfabeto m)) (estados m)

trans_eps_to_afn_aux :: AFNEp -> [Trans_eps] -> String -> String -> [Trans_afn]
trans_eps_to_afn_aux _ _ [] _ = []
trans_eps_to_afn_aux m l (c:cs) q = (q, c, qn) : (trans_eps_to_afn_aux m l cs q)
  where qn = eclosure2 m $ do_trans_nep2 l c (eclosure l m q)

eclosure ::  [Trans_eps] -> AFNEp -> String  -> [String]
eclosure [] _ q1 = [q1]
eclosure ((q2, Nothing, l):xs) m q1
  | q2 == q1 = rmDup $ (q1:l) ++ eclosure2 m l
  | otherwise = eclosure xs m q1
eclosure (x:xs) m q1  = eclosure xs m q1

eclosure2 ::  AFNEp -> [String]  -> [String]
eclosure2 _ [] = []
eclosure2 m (x:xs) = eclosure (transiciones m) m x ++ eclosure2 m xs

rmDup :: (Ord a) => [a] -> [a]
rmDup = toList . fromList 

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

do_trans_nep :: [Trans_eps] -> Char -> String -> [String]
do_trans_nep [] _ _ = [""]
do_trans_nep ((q1, c1, q2):xs) c2 q3
  | q1 == q3 && (to_char c1) == c2 = q2
  | otherwise            = do_trans_nep xs c2 q3

to_char :: Maybe Char -> Char
to_char Nothing = '~'
to_char (Just a) = a

do_trans_nep2 :: [Trans_eps] -> Char -> [String] -> [String]
do_trans_nep2 l c qs = formato $ map (do_trans_nep l c) qs


formato :: [[String]] -> [String]
formato [] = []
formato (x:xs)
  | x == [""]   = formato xs
  | otherwise =  x++formato xs