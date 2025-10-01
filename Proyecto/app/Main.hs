module Main (main) where

import AFN
import AFNe
import Regex

e1 :: Expr
e1 = And (Kleene (Or (Term '0') (Term '1'))) (Term '1')

a1 :: AFNEp
a1 = expr_to_AFNEp e1

a2 :: AFN
a2 = afnEp_to_AFN a1

e2 :: Expr
e2 = And (And (Term 'f') (Term 'o')) (Term 'r')

a3 :: AFNEp
a3 = expr_to_AFNEp e2

a4 :: AFN
a4 = afnEp_to_AFN a3

main :: IO ()
main = do
    putStrLn("Expresión Regular:")
    print(e2)
    putStrLn("\n\n Autómata con transiciones epsilon:")
    print(a3)
    putStrLn("\n\n Autómata no determinista:")
    print(a4)
    putStrLn("\n \n Ejemplo 2- Expresión Regular:")
    print(e1)
    putStrLn("\n\n Autómata con transiciones epsilon:")
    print(a1)
    putStrLn("\n\n Autómata no determinista:")
    print(a2)

