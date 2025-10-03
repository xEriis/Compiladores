module Main (main) where

import AFD
import AFN
import AFNe
import Regex
import ReadFile
import System.IO

e0 :: Expr
e0 = string_to_regex "Concat (Kleene (Or (Term 0) (Term 1))) (Term 1)"

a0 :: AFNe
a0 = regex_to_AFNe e0

a02 :: AFN
a02 = afnEp_to_AFN a0

e1 :: Expr
e1 = Concat (Kleene (Or (Term '0') (Term '1'))) (Term '1')

a1 :: AFNe
a1 = regex_to_AFNe e1

a2 :: AFN
a2 = afnEp_to_AFN a1

e2 :: Expr
e2 = Concat (Concat (Term 'f') (Term 'o')) (Term 'r')

a3 :: AFNe
a3 = regex_to_AFNe e2

a4 :: AFN
a4 = afnEp_to_AFN a3

-- Paso de conversión determinista
a5 :: AFD
a5 = afn_to_AFD a4


main :: IO ()
main = do
    fileHandle <- openFile "./specs/IMP.md" ReadMode
    contents <- hGetContents fileHandle
    let n = handle_contents2 contents
    putStrLn("\nExpresiones leídas del archivo : \n")
    print(n)
    putStrLn("\n")
    putStrLn("Prueba para leer expresiones regulares:")
    print(e0)
    print(a0)
    print(a02)
    putStrLn("\n\n Expresión Regular:")
    print(e2)
    putStrLn("\n\n Autómata con transiciones epsilon:")
    print(a3)
    putStrLn("\n\n Autómata no determinista:")
    print(a4)
    -- Aqui proximamente tendría que salir la conversión determinista
    putStrLn("\n\n Autómata determinista:")
    print(a5)

    putStrLn("\n \n Ejemplo 2- Expresión Regular:")
    print(e1)
    putStrLn("\n\n Autómata con transiciones epsilon:")
    print(a1)
    putStrLn("\n\n Autómata no determinista:")
    print(a2)
    hClose fileHandle

