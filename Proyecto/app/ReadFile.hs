module ReadFile 
(
    string_to_regex,
    handle_contents,
    handle_contents2,
    handle_contents3,
    handle_contents4,
    handle_specs,
    remove_comments_test
)
where

import GHC.Unicode
import Regex
import Data.List (isPrefixOf)

------------------------------------------------------------------------------------------
-- Función para manejar una cadena leída del archivo y convertirla a una expresión regular
------------------------------------------------------------------------------------------
handle_contents :: [Char] -> Expr
handle_contents x = string_to_regex x

------------------------------------------------------------------------------------------------------------
-- Función que lee expresiones regulares de un archivo y las convierte a una lista de expresiones regulares
-- El archivo debe tener únicamente una expresión regular por línea.
-- En cada línea incluir únicamente la expresión regular y espacios, nada más
-------------------------------------------------------------------------------------------------------------
handle_contents2 :: [Char] -> [Expr]
handle_contents2 [] = []
handle_contents2 x =  [string_to_regex $ line_content x []] ++ (handle_contents2 $ skip_line x)

-----------------------------------------------------------------------------------------------------------------------------------------
-- Función que dado un archivo con expresiones regulares, regresa la indicada con un caracter
-- El arhcivo debe de indicar las expresiones regulares con un caracter(id) seguido de un '=' y la expresión regular con o sin espacios
-- Por ejemplo:
-- a= Concat [Term 1] [Term 2]
-- b= [Term 1]
-- c= [term 2]
-- La función recibe como parámetro el id (a,b,c)
------------------------------------------------------------------------------------------------------------------------------------------
handle_contents3 :: [Char] -> Char -> Expr
handle_contents3 [] _ = error "Error al leer el archivo"
handle_contents3 (x:xs) c
    | x == c && cabeza_e xs == '=' = string_to_regex $ cdr $ line_content xs []
    | otherwise = handle_contents3  (skip_line xs) c

--------------------------------------------------------------------------------------------------
-- Función que dada una cadena, regresa el contenido después del primer salto de línea encontrado
--------------------------------------------------------------------------------------------------
skip_line :: [Char] -> [Char]
skip_line [] = []
skip_line (x:xs)
    | x == '\n' = xs
    | otherwise = skip_line xs 


------------------------------------------------------------------------------------------------
-- Función que regresa el contendio de una cadena anterior al primer salto de línea encontrado
------------------------------------------------------------------------------------------------
line_content :: [Char] -> [Char] -> [Char]
line_content [] p = p
line_content (x:xs) p
    | x == '\n' = p
    | otherwise = line_content xs (p++[x])

---------------------------------------------------------
-- Función que recibe una cadena y la tranforma al Data Expr
-- El formato para las expresiones regulares es:
-- OR [lw] [rw] 
-- Concat [lw] [rw]
-- Kleene [lw]
-- Term x
-- Donde x es un caracter
-- lw y rw son expresiones regulares
---------------------------------------------------------
string_to_regex :: [Char] -> Expr
string_to_regex [] = error "Formato incorrecto"
string_to_regex (x:xs)
    | isSpace x = string_to_regex xs
    | x == '[' = string_to_regex xs
    | x == 'T' = Term (find_term (x:xs))
    | x == 'O' = Or (string_to_regex y) (string_to_regex y1)
    | x == 'C' = Concat (string_to_regex y) (string_to_regex y1)
    | x == 'K' = Kleene (string_to_regex y)
    | otherwise = error "Formato incorrecto"
    where y = cabeza( tres xs 0 [] [])
          y1 = cabeza $ cdr $ tres xs 0 [] []

---------------------------------------------
-- Función que regresa la cabeza de una lista
---------------------------------------------
cabeza :: [a]-> a
cabeza [] = error "Formato incorrecto"
cabeza (x:_) = x


---------------------------------------------
-- Función que regresa el resto de una lista
---------------------------------------------
cdr :: [a]->[a]
cdr [] = error "Formato incorrecto"
cdr (_:xs) = xs


----------------------------------------------------
-- Funcion que regesa el caracter asociado a un Term
----------------------------------------------------
find_term :: [Char] -> Char
find_term [] = error "Formato incorrecto"
find_term (x:xs) 
    | x == 'T' = find_term xs
    | x == 'e' = find_term xs
    | x == 'r' = find_term xs
    | x == 'm' = cabeza_e xs
    | otherwise = error "Formato incorrecto"

------------------------------------------------------------------
-- Función que regresa la cabeza de una string, ignorando espacios
------------------------------------------------------------------
cabeza_e :: [Char] -> Char
cabeza_e [] = error "Formato incorrecto"
cabeza_e (x:xs)
    | isSpace x = cabeza_e xs
    | otherwise = x

-----------------------------------------------------------------------------------------------------------
-- Función que parte en tres una cadena, ignorando espacios y separando por parentesis bien equilibrados
-----------------------------------------------------------------------------------------------------------
tres :: [Char] -> Int -> [Char] -> [[Char]] -> [[Char]]
tres [] _ _ _ = []
tres (x:xs) n p1 p
    | x == '[' && n == 0 = tres xs (n+1) (p1) p
    | x == '[' = tres xs (n+1) (p1++[x]) p
    | x == ']' && n == 1 = (p++[p1]++(tres xs 0 [] []))
    | x == ']' = tres xs (n-1) (p1++[x]) p
    | isSpace x = tres xs n p1 p
    | n == 0 = tres xs n p1 p
    | otherwise = tres xs n (p1++[x]) p


-- Quita espacios del inicio y final
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Detecta comentarios
es_comentario :: String -> Bool
es_comentario l =
  let t = trim l
  in null t || "--" `isPrefixOf` t

----------------------------------------------------------
-- handle_contents3 modificado:
-- Ahora también ignora comentarios "--"
-- y busca por nombre de token (String), no solo Char.
----------------------------------------------------------
handle_contents4 :: String -> String -> Expr
handle_contents4 [] _ = error "Error al leer el archivo"
handle_contents4 texto nombre =
  buscar_linea (lines texto)
  where
    buscar_linea [] = error ("Token '" ++ nombre ++ "' no encontrado")
    buscar_linea (l:ls)
      | es_comentario l = buscar_linea ls
      | otherwise =
          let limpio = trim l
          in if take (length nombre) limpio == nombre && elem '=' limpio
                then 
                    let 
                    resto = drop1 $ dropWhile (/= '=') limpio
                    bloque = unlines (resto : takeWhile es_parte_expr ls)
                    in string_to_regex (trim bloque)
                else buscar_linea ls

----------------------------------------------------------
-- handle_specs:
-- Lee todas las líneas de un archivo de especificaciones IMP
-- Devuelve [(nombreToken, Expr)]
-- Ignora comentarios y espacios en blanco
----------------------------------------------------------
handle_specs :: String -> [(String, Expr)]
handle_specs texto =
  let ls = lines texto
      lsValidas = filter (not . es_comentario) ls
      lineasConExpr = filter (\l -> '=' `elem` l && not (null (trim l))) lsValidas
      parseLinea l =
        let (nombre, resto) = break (=='=') l
        in (trim nombre, string_to_regex (trim (drop1 resto)))
  in map parseLinea lineasConExpr

----------------------------------------------------------
-- drop1: versión segura de tail
----------------------------------------------------------
drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_:xs) = xs

----------------------------------------------------------
-- Determina si una línea sigue siendo parte de la expresión
----------------------------------------------------------
es_parte_expr :: String -> Bool
es_parte_expr l =
  let t = trim l
  in not (null t) && not (take 2 t == "--")


----------------------------------------------------------------------------
-- Para comentarios del lenguaje IMP (que pasen por entrada en los tests):
----------------------------------------------------------------------------

------------------------------------------------------------
-- Función remove_comments_test
-- Elimina los comentarios de una cadena fuente,
-- ignorando tanto comentarios de línea (// ...) y
-- comentarios de bloque (/* ... */).
-- Se recorre la cadena carácter por carácter:
--   * Si se detecta "//", se llama a skipLine
--   * Si se detecta "/*", se llama a skipBlock
--   * En cualquier otro caso, se conserva el carácter
------------------------------------------------------------
remove_comments_test :: String -> String
remove_comments_test [] = []
remove_comments_test ('/':'/':xs) = skip_line2 xs              -- ignoramos hasta el salto de línea
remove_comments_test ('/':'*':xs) = skip_block xs             -- ignoramos hasta */
remove_comments_test (x:xs) = x : remove_comments_test xs      -- conservamos todo lo demás

------------------------------------------------------------
-- Función skip_line2
-- Ignora todo el comentario de línea
------------------------------------------------------------
skip_line2 :: String -> String
skip_line2 [] = []
skip_line2 ('\n':xs) = '\n' : remove_comments_test xs
skip_line2 (_:xs) = skip_line2 xs

------------------------------------------------------------
-- Función skip_lock
-- Ignora caracteres mientras no encuentre el patrón "*/" 
-- que marca el final del comentario de bloque.
------------------------------------------------------------
skip_block :: String -> String
skip_block [] = []  -- si nunca se cierra, ignora hasta el final
skip_block ('*':'/':xs) = remove_comments_test xs
skip_block (_:xs) = skip_block xs