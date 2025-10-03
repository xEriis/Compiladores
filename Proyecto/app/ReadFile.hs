module ReadFile 
(
    string_to_regex,
    handle_contents,
    handle_contents2,
    handle_contents3
)
where

import GHC.Unicode
import Regex

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
-- a= Concat (Term 1) (Term 2)
-- b= (Term 1)
-- c= (term 2)
-- La función recibe como parámetro el id (a,b,c)
------------------------------------------------------------------------------------------------------------------------------------------
handle_contents3 :: [Char] -> Char -> Expr
handle_contents3 [] _ = error "Error al leer el archivo"
handle_contents3 (x:xs) c
    | x == c && cabeza_e xs == '=' = string_to_regex $ cdr $ line_content xs []
    | otherwise = handle_contents3  (skip_line xs) c

--------------------------------------------------------------------------------------------------
-- FUnción que dada una cadena, regresa el contenido después del primer salto de línea encontrado
--------------------------------------------------------------------------------------------------
skip_line :: [Char] -> [Char]
skip_line [] = []
skip_line (x:xs)
    | x == '\n' = xs
    | otherwise = skip_line xs 


------------------------------------------------------------------------------------------------
-- FUnción que regresa el contendio de una cadena anterior al primer salto de línea encontrado
------------------------------------------------------------------------------------------------
line_content :: [Char] -> [Char] -> [Char]
line_content [] p = p
line_content (x:xs) p
    | x == '\n' = p
    | otherwise = line_content xs (p++[x])

---------------------------------------------------------
-- Función que recibe una cadena y la tranforma al Data Expr
-- El formato para las expresiones regulares es:
-- OR (lw) (rw) 
-- Concat (lw) (rw)
-- Kleene (lw)
-- Term x
-- Donde x es un caracter
-- lw y rw son expresiones regulares
---------------------------------------------------------
string_to_regex :: [Char] -> Expr
string_to_regex [] = error "Formato incorrecto"
string_to_regex (x:xs)
    | isSpace x = string_to_regex xs
    | x == '(' = string_to_regex xs
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
    | x == '(' && n == 0 = tres xs (n+1) (p1) p
    | x == '(' = tres xs (n+1) (p1++[x]) p
    | x == ')' && n == 1 = (p++[p1]++(tres xs 0 [] []))
    | x == ')' = tres xs (n-1) (p1++[x]) p
    | isSpace x = tres xs n p1 p
    | n == 0 = tres xs n p1 p
    | otherwise = tres xs n (p1++[x]) p
