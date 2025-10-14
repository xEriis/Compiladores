------------------------------------------------------
-- Expresiones regulares de IMP
-- Cada línea define un token con el formato:
-- nombre_token = <Expresión Regular en formato Haskell>
------------------------------------------------------


------------------------------------------------------
-- Identificadores:  @ @* # 
-- @ = letras mayúsculas o minúsculas
-- # = números del 1-9
------------------------------------------------------
id = Concat 
          [Term @ ]
          [Concat
            [Kleene [Term @]]
            [Kleene  [Term #]]]

------------------------------------------------------
-- Números: 0 | # (0 | #)* | - # (0 | #)*
-- # = números del 1-9
------------------------------------------------------
num = Or 
        [Term 0]
        [Or 
          [Concat 
            [Term #]
            [Kleene [Or [Term 0] [Term #]]]]
          [Concat 
            [Term -]
            [Concat 
            [Term #]
            [Kleene [Or [Term 0] [Term #]]]]
          ]
        ]

------------------------------------------------------
-- Operadores aritméticos: + | - | * | /
------------------------------------------------------
op_arit = Or 
            [Term +] 
            [Or [Term -] [Or [Term *] [Term /]]]

------------------------------------------------------
-- Asignación: :=
------------------------------------------------------
asign = Concat [Term :] [Term =]

------------------------------------------------------
-- Operadores relacionales: < | > | =
------------------------------------------------------
op_rel = Or 
          [Term <] 
          [Or [Term >] [Term =]]


------------------------------------------------------
-- Palabras reservadas de Condicionales
------------------------------------------------------
res_cond = Or
            [Concat [Term i] [Term f]]
            [Or 
              [Concat [Term t] [Concat [Term h] [Concat [Term e] [Term n]]]]
              [Concat [Term e] [Concat [Term l] [Concat [Term s] [Term e]]]]]


------------------------------------------------------
-- Palabras reservadas de Ciclos
------------------------------------------------------
res_cicle = Or
            [Concat [Term w] [Concat [Term h] [Concat [Term i] [Concat [Term l] [Term e]]]]]
            [Or 
              [Concat [Term d] [Term o]]
              [Concat [Term f] [Concat [Term o] [Term r]]]]

------------------------------------------------------
-- Palabras reservadas extra
------------------------------------------------------
res_extra = Concat [Term s] [Concat [Term k] [Concat [Term i] [Term p]]]

------------------------------------------------------
-- Puntuación
------------------------------------------------------
punt = Term ;

------------------------------------------------------
-- Delimitadores
------------------------------------------------------
delim = Or 
          [Term {]
          [Or [Term }] [Or [Term (] [Term )]]]

------------------------------------------------------
-- Booleanos
------------------------------------------------------
bool = Or
          [Concat [Term t] [Concat [Term r] [Concat [Term u] [Term e]]]]
          [Concat [Term f] [Concat [Term a] [Concat [Term l] [Concat [Term s] [Term e]]]]]
                              
------------------------------------------------------
-- Operadores booleanos
------------------------------------------------------
op_bool = Or
            [Concat [Term n] [Concat [Term o] [Term t]]]
            [Or [Concat [Term a] [Concat [Term n] [Term d]]]
                [Concat [Term o] [Term r]]]

------------------------------------------------------
-- Comentarios: estos se ignoran en el lenguaje
------------------------------------------------------