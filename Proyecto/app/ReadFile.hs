module ReadFile 
(
    uno,
    dos,
    append,
    string_to_regex
)
where

import GHC.Unicode
import Regex



string_to_regex :: [Char] -> Expr
string_to_regex [] = error "Formato incorrecto"
string_to_regex (x:xs) 
    | x == '(' = string_to_regex xs
    | x == 'T' = Term (find_term (x:xs))
    | x == 'O' = Or (string_to_regex y) (string_to_regex y1)
    | x == 'C' = Concat (string_to_regex y) (string_to_regex y1)
    | x == 'K' = Kleene (string_to_regex y)
    | otherwise = error "Formato incorrecto"
    where y = cabeza( tres xs 0 [] [])
          y1 = cabeza $ cdr $ tres xs 0 [] []

cabeza :: [a]-> a
cabeza [] = error "Formato incorrecto"
cabeza (x:_) = x

cdr :: [[a]]->[[a]]
cdr [] = error "Formato incorrecto"
cdr (_:xs) = xs

find_term :: [Char] -> Char
find_term [] = error "Formato incorrecto"
find_term (x:xs) 
    | x == 'T' = find_term xs
    | x == 'e' = find_term xs
    | x == 'r' = find_term xs
    | x == 'm' = cabeza_e xs
    | otherwise = error "Formato incorrecto"

cabeza_e :: [Char] -> Char
cabeza_e [] = error "Formato incorrecto"
cabeza_e (x:xs)
    | isSpace x = cabeza_e xs
    | otherwise = x

uno :: [Char] -> [Char]
uno [] = []
uno (x:xs)
    | x == '(' = dos xs 1 []
    | otherwise = uno xs


dos :: [Char] -> Int -> [Char] -> [Char]
dos _ 0 p = p
dos [] _ _ = []
dos (x:xs) n p 
    | x == '(' = dos xs (n+1) (p++[x])
    | x == ')' && n == 1 = p
    | x == ')' = dos xs (n-1) (p ++ [x])
    | isSpace x = dos xs n p
    | otherwise = dos xs n (p ++ [x])

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

append :: [[a]] -> [a] -> [[a]]
append [] y = [y]
append (x:xs) y = x : append xs y