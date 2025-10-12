module Main (main) where

import ReadFile
import Regex
import AFNe
import AFN
import AFD
import MDD
import Lexer
import Data.List (nub)


import System.IO

main :: IO ()
main = do
    -- Leemos las especificaciones desde IMP.md
    fileHandle <- openFile "./specs/IMP.md" ReadMode
    contents <- hGetContents fileHandle

    putStrLn("\nExpresiones leídas del archivo IMP.md:")

    -- Leemos tokens individuales con handle_contents4
    let expr_id = handle_contents4 contents "id"
    let expr_num = handle_contents4 contents "num"
    let expr_op_a = handle_contents4 contents "op_arit"
    let expr_asig = handle_contents4 contents "asign"
    let expr_op_rel = handle_contents4 contents "op_rel"
    let expr_res = handle_contents4 contents "res"
    let expr_punt = handle_contents4 contents "punt"
    let expr_delim = handle_contents4 contents "delim"
    let expr_bool = handle_contents4 contents "bool"
    let expr_op_bool = handle_contents4 contents "op_bool"
    let expr_com = handle_contents4 contents "com"

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

    putStrLn "\nres:"
    print expr_res

    putStrLn "\npunt:"
    print expr_punt

    putStrLn "\ndelim:"
    print expr_delim

    putStrLn "\nbool:"
    print expr_bool

    putStrLn "\nop_bool:"
    print expr_op_bool

    putStrLn "\ncom:"
    print expr_com

    -- Construimos la lista completa:
    let tokensPrueba = [("id", expr_id), ("num", expr_num), ("op_arit", expr_op_a), ("asign", expr_asig), ("op_rel", expr_op_rel), ("res", expr_res), ("punt", expr_punt), ("delim", expr_delim), ("bool", expr_bool), ("op_bool", expr_op_bool), ("com", expr_com)]

    ------------------------------------------
    -- Solo para probar palabras reservadas:
    -- let tokensPrueba = [("id", expr_id)]
    ------------------------------------------

    -- let tokensPrueba = [("res", expr_res)] 
    -- 1. ER → AFN-epsilon → AFN → AFD → AFDmin
    -------------------------------------------------------
    -- PARA VERIFICAR QUE TODO EL PIPELINE PASADO FUNCIONE
    ------------------------------------------------------

    -- putStrLn "\n=== Construyendo AFNe, AFN, AFD (raw) y AFD min para cada token (DEBUG) ==="

    -- -- Construimos paso a paso y guardamos todo para imprimir
    -- -- función local para detectar duplicados (mismo (estado, símbolo) -> varios destinos)
    -- let detectDups ts = [ ((q,c), nub [ qn | (q',c',qn) <- ts, q' == q, c' == c ])
    --                         | (q,c,_) <- ts, let ds = nub [ qn | (q',c',qn) <- ts, q' == q, c' == c ], length ds > 1
    --                     ]

    -- -- función local para detectar duplicados
    -- let detectDups ts =
    --       [ ((q,c), nub [ qn | (q',c',qn) <- ts, q' == q, c' == c ])
    --       | (q,c,_) <- ts
    --       , let ds = nub [ qn | (q',c',qn) <- ts, q' == q, c' == c ]
    --       , length ds > 1
    --       ]

    -- -- Para cada token, construye y muestra AFNe, AFN, AFD raw y AFD minimizado
    -- mapM_ (\(name, expr) -> do
    --     putStrLn $ "\n--- Token: " ++ name ++ " ---"
    --     putStrLn "Regex (original):"
    --     print expr

    --     -- construir las máquinas paso a paso
    --     let afne   = regex_to_AFNe expr
    --         afn    = afnEp_to_AFN afne
    --         afdRaw = afn_to_AFD afn
    --         afdMin = minimiza afdRaw

    --     putStrLn "\nAFNe (regex_to_AFNe):"
    --     --print afne

    --     putStrLn "\nAFN (afnEp_to_AFN):"
    --     -- print afn

    --     putStrLn "\nAFD (afn_to_AFD):"
    --     -- print afdRaw

    --     putStrLn "\nAFD minimizado (minimiza):"
    --     -- print afdMin

    --     -- detectar duplicados en el AFD raw y en el AFDmin
    --     let dupRaw = detectDups (transicionesD afdRaw)
    --         dupMin = detectDups (transicionesD afdMin)

    --     if null dupRaw
    --         then putStrLn "\n  No se detectaron duplicados en AFD."
    --         else do
    --             putStrLn "\n  Duplicados en AFD:"
    --             mapM_ print dupRaw

    --     if null dupMin
    --         then putStrLn "  No se detectaron duplicados en AFD minimizado."
    --         else do
    --             putStrLn "  Duplicados en AFD minimizado:"
    --             mapM_ print dupMin

    --     ) tokensPrueba

    -- putStrLn "\n=== Construcción de AFDs para cada token ==="
    -- let afds = [ (tok, minimiza (afn_to_AFD (afnEp_to_AFN (regex_to_AFNe expr)))) | (tok, expr) <- tokens ]
    -- let afds = [ (tok, minimiza (afn_to_AFD (afnEp_to_AFN (regex_to_AFNe expr)))) | (tok, expr) <- tokensPrueba ]

    -- Generar AFDs min
    let afds = [ (name, minimiza (afn_to_AFD (afnEp_to_AFN (regex_to_AFNe expr))))
                     | (name, expr) <- tokensPrueba ]

    -- Mostrar cada AFDmin (transiciones y finales) antes de construir la MDD
    -- mapM_ (\(name, afdMin) -> do
    --         putStrLn $ "=== AFD para token: " ++ name ++ " ==="
    --         putStrLn $ "Estados: " ++ show (estadosD afdMin)
    --         putStrLn $ "Alfabeto: " ++ show (alfabetoD afdMin)
    --         putStrLn $ "Transiciones: "
    --         mapM_ print (transicionesD afdMin)
    --         putStrLn $ "Finales: " ++ show (finalesD afdMin)
    --         -- detectar transiciones duplicadas (mismo (estado, simbolo) -> múltiples destinos)
    --         let pairs = [ ((q,c), qn) | (q,c,qn) <- transicionesD afdMin ]
    --             grouped = foldr (\(k,v) acc -> case lookup k acc of
    --                                             Nothing -> (k,[v]) : acc
    --                                             Just vs -> (k, v:vs) : filter ((/= k) . fst) acc) [] pairs
    --             duplicates = filter (\(_,vs) -> length vs > 1) grouped
    --         if null duplicates
    --         then putStrLn "No se detectaron transiciones duplicadas."
    --         else do putStrLn "Transiciones duplicadas detectadas (estado,simbolo) -> [destinos]:"
    --                 mapM_ print duplicates
    --     ) afds
    
    -- -- Debug: probar directamente el AFD de op_arit
    -- let Just afdOpA = lookup "op_arit" afds
    -- putStrLn "\n=== Prueba directa del AFD de op_arit ==="
    -- print afdOpA

    -- -- Simulamos directamente el símbolo '+'
    -- let estadoInicial = inicialD afdOpA
    -- let trans = transicionesD afdOpA
    -- let destino = [q2 | (q1,c,q2) <- trans, q1 == estadoInicial, c == '-']
    -- putStrLn $ "Desde el estado inicial con '-': " ++ show destino

    -- let Just afdA = lookup "asign" afds
    -- putStrLn "\n=== Prueba directa del AFD de asign ==="
    -- print afdA

    -- -- Simulamos directamente el símbolo '+'
    -- let estadoInicial = inicialD afdA
    -- let trans = transicionesD afdA
    -- let destino = [q2 | (q1,c,q2) <- trans, q1 == estadoInicial, c == ':']
    -- putStrLn $ "Desde el estado inicial con '-': " ++ show destino

    

    -- Mostrar resumen
    -- mapM_ (\(n,a) -> putStrLn $ "Token: " ++ n ++ "  estados: " ++ show (length (estadosD a))) afds

    -- 2. Construimos la MDD con todos los AFDs
    let mdd = buildMDD afds
    putStrLn "\nTransiciones desde MDD_0:"
    mapM_ print [t | t@(q0,_,_) <- transicionesM mdd, q0 == inicialM mdd]

    -- PARA TRANSICIONES DUPLICADAS:
    -- let trans = transicionesM mdd
    -- let groupedM = foldr (\(q,c,qn) acc -> case lookup (q,c) acc of
    --                                         Nothing -> ((q,c),[qn]) : acc
    --                                         Just vs -> ((q,c), qn:vs) : filter ((/= (q,c)) . fst) acc) [] trans
    -- let dupsM = filter (\(_,vs) -> length vs > 1) groupedM
    -- putStrLn "Duplicados en la MDD ( (estado,simbolo) -> [destinos] ):"
    -- mapM_ print dupsM

    putStrLn "\n=== Debug de la MDD ==="
    putStrLn "\nTransiciones:"
    print (transicionesM mdd)   -- busca ("MDD_START",'i', ...)
    putStrLn "\nFinales:"
    print (finalesM mdd)

    putStrLn "\nResumen de la MDD:"
    putStrLn $ "  Estados totales:     " ++ show (length (estadosM mdd))
    putStrLn $ "  Transiciones totales:" ++ show (length (transicionesM mdd))
    putStrLn $ "  Estados finales:     " ++ show (length (finalesM mdd))
    
    putStrLn "MDD construida correctamente.\n"

    ---------------------------------------------
    -- PRUEBA
    ----------------------------------------
    putStrLn "\n=== Prueba del lexer ==="
    putStrLn "\nAnalizando cadena de prueba:"
    let entrada = "{}(){}3+1id for ifa if(true){x1:=x2+2}//hola;"
    putStrLn $ "  " ++ show entrada
    
    putStrLn "DEBUG: llamar a longestMatchM directamente:"
    print (longestMatchM mdd entrada)

    putStrLn "\nTokens reconocidos:"
    let resultado = lexerM mdd entrada
    mapM_ print resultado

    -- -- 3. Analizamos un programa IMP
    -- putStrLn "\nAnalizando archivo ./samples/ejemplo1.imp"
    -- program <- readFile "./samples/ejemplo1.imp"

    -- putStrLn "\nCódigo fuente:"
    -- putStrLn program

    -- let tokensReconocidos = lexerM mdd program
    -- putStrLn "\nTokens encontrados:"
    -- mapM_ print tokensReconocidos
    

    hClose fileHandle

