module Main (main) where

import ReadFile
import Regex
import AFNe
import AFN
import AFD
import MDD
import Lexer

import System.IO

-----------------------------------------------------------
-- Función Auxiliar que ejecuta la función lexer y los imprime
-- Recibe la ruta del archivo que contienen las sentencias
-- Recibe la MDD ya construida
-----------------------------------------------------------
example :: [Char] -> MDD -> IO()
example path mdd = do
    putStr "\nAnalizando archivo:"
    print path
    program1 <- readFile path

    -- Limpiamos comentarios antes de analizar
    let programSinComentarios1 = remove_comments_test program1

    putStrLn "\nCódigo fuente original:"
    putStrLn program1
    putStrLn "\nCódigo sin comentarios:"
    putStrLn programSinComentarios1

    let tokensReconocidos1 = lexerM mdd programSinComentarios1
    putStrLn "\nTokens encontrados:"
    mapM_ print tokensReconocidos1

main :: IO ()
main = do
    -- 1. Leemos las especificaciones desde IMP.md
    fileHandle <- openFile "./specs/IMP.md" ReadMode
    contents <- hGetContents fileHandle

    putStrLn("\nExpresiones leídas del archivo IMP.md:")

    -- Leemos tokens individuales con handle_contents4
    let expr_id = handle_contents4 contents "id"
    let expr_num = handle_contents4 contents "num"
    let expr_op_a = handle_contents4 contents "op_arit"
    let expr_asig = handle_contents4 contents "asign"
    let expr_op_rel = handle_contents4 contents "op_rel"
    let expr_res_cond = handle_contents4 contents "res_cond"
    let expr_res_cicle = handle_contents4 contents "res_cicle"
    let expr_res_extra = handle_contents4 contents "res_extra"
    let expr_punt = handle_contents4 contents "punt"
    let expr_delim = handle_contents4 contents "delim"
    let expr_bool = handle_contents4 contents "bool"
    let expr_op_bool = handle_contents4 contents "op_bool"

    -- Las imprimimos:
    putStrLn "\nid:"
    print expr_id

    putStrLn "\nnum:"
    print expr_num

    putStrLn "\nop_arit:"
    print expr_op_a

    putStrLn "\nasign:"
    print expr_asig

    putStrLn "\nop_rel:"
    print expr_op_rel

    putStrLn "\nres_cond:"
    print expr_res_cond

    putStrLn "\nres_cicle:"
    print expr_res_cicle

    putStrLn "\nres_extra:"
    print expr_res_extra

    putStrLn "\npunt:"
    print expr_punt

    putStrLn "\ndelim:"
    print expr_delim

    putStrLn "\nbool:"
    print expr_bool

    putStrLn "\nop_bool:"
    print expr_op_bool

    -- Construimos la lista completa:
    let tokens = [("id", expr_id), ("num", expr_num), ("op_arit", expr_op_a), ("asign", expr_asig), ("op_rel", expr_op_rel), ("res_cond",expr_res_cond),("res_cicle",expr_res_cicle),("res_extra",expr_res_extra), ("punt", expr_punt), ("delim", expr_delim), ("bool", expr_bool), ("op_bool", expr_op_bool)]

    -- 1. ER → AFN-epsilon → AFN → AFD → AFDmin

    -- Generar AFDs min
    let afds = [ (name, minimiza (afn_to_AFD (afnEp_to_AFN (regex_to_AFNe expr))))
                     | (name, expr) <- tokens ]


    -- 2. Construimos la MDD con todos los AFDs
    let mdd = buildMDD afds

    putStrLn "MDD construida correctamente.\n"

    putStrLn "\nResumen de la MDD:"
    putStrLn $ "  Estados totales:     " ++ show (length (estadosM mdd))
    putStrLn $ "  Transiciones totales:" ++ show (length (transicionesM mdd))
    putStrLn $ "  Estados finales:     " ++ show (length (finalesM mdd))

    -- 3. Analizamos programas IMP
    putStr "\n---- INGRESE EL NÚMERO DEL ARCHIVO DE PRUEBA (1,2,3,4,5)----\n"
    input <- getLine
    let num = read input :: Int
    case num of
        1 -> example "./samples/ejemplo1.imp" mdd
        2 -> example "./samples/ejemplo2.imp" mdd
        3 -> example "./samples/ejemplo3.imp" mdd
        4 -> example "./samples/ejemplo4.imp" mdd
        5 -> example "./samples/ejemplo5.imp" mdd
        _ -> error "No es un número válido"
    
    
    hClose fileHandle

