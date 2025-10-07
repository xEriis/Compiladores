------------------------------------------------------
-- Expresiones regulares de IMP
-- Cada línea define un token con el formato:
-- nombre_token = <Expresión Regular en formato Haskell>
------------------------------------------------------


------------------------------------------------------
-- Identificadores:  A (A|D)* 
-- A = letras mayúsculas o minúsculas
-- D = dígitos del 0 al 9
------------------------------------------------------
id = Concat 
        (Or (Term a) (Or (Term b) (Or (Term c) (Or (Term d)
          (Or (Term e) (Or (Term f) (Or (Term g) (Or (Term h)
          (Or (Term i) (Or (Term j) (Or (Term k) (Or (Term l)
          (Or (Term m) (Or (Term n) (Or (Term ñ) (Or (Term o)
          (Or (Term p) (Or (Term q) (Or (Term r) (Or (Term s)
          (Or (Term t) (Or (Term u) (Or (Term v) (Or (Term w)
          (Or (Term x) (Or (Term y) (Or (Term z) (Or (Term A)
          (Or (Term B) (Or (Term C) (Or (Term D) (Or (Term E)
          (Or (Term F) (Or (Term G) (Or (Term H) (Or (Term I)
          (Or (Term J) (Or (Term K) (Or (Term L) (Or (Term M)
          (Or (Term N) (Or (Term Ñ) (Or (Term O) (Or (Term P)
          (Or (Term Q) (Or (Term R) (Or (Term S) (Or (Term T)
          (Or (Term U) (Or (Term V) (Or (Term W) (Or (Term X)
          (Or (Term Y) (Term Z)))))))))))))))))))))))))))))))))))))))))))))))))))))
        )
        (Kleene 
          (Or 
            (Or (Term a) (Or (Term b) (Or (Term c) (Or (Term d)
              (Or (Term e) (Or (Term f) (Or (Term g) (Or (Term h)
              (Or (Term i) (Or (Term j) (Or (Term k) (Or (Term l)
              (Or (Term m) (Or (Term n) (Or (Term ñ) (Or (Term o)
              (Or (Term p) (Or (Term q) (Or (Term r) (Or (Term s)
              (Or (Term t) (Or (Term u) (Or (Term v) (Or (Term w)
              (Or (Term x) (Or (Term y) (Or (Term z) (Or (Term A)
              (Or (Term B) (Or (Term C) (Or (Term D) (Or (Term E)
              (Or (Term F) (Or (Term G) (Or (Term H) (Or (Term I)
              (Or (Term J) (Or (Term K) (Or (Term L) (Or (Term M)
              (Or (Term N) (Or (Term Ñ) (Or (Term O) (Or (Term P)
              (Or (Term Q) (Or (Term R) (Or (Term S) (Or (Term T)
              (Or (Term U) (Or (Term V) (Or (Term W) (Or (Term X)
              (Or (Term Y) (Term Z))))))))))))))))))))))))))))))))))))))))))))))))))))))
            (Or (Term 0) (Or (Term 1) (Or (Term 2) (Or (Term 3)
              (Or (Term 4) (Or (Term 5) (Or (Term 6) (Or (Term 7)
              (Or (Term 8) (Term 9))))))))))
          )
        )

------------------------------------------------------
-- Números: 0 | ZD* | -ZD*
-- Z = 1..9, D = 0..9
------------------------------------------------------
num = Or 
        (Term 0)
        (Or 
          (Concat 
            (Or (Term 1) (Or (Term 2) (Or (Term 3) (Or (Term 4) (Or (Term 5) (Or (Term 6) (Or (Term 7) (Or (Term 8) (Term 9)))))))))
            (Kleene (Or (Term 0) (Or (Term 1) (Or (Term 2) (Or (Term 3) (Or (Term 4) (Or (Term 5) (Or (Term 6) (Or (Term 7) (Or (Term 8) (Term 9))))))))))))
          (Concat 
            (Term -)
            (Concat 
              (Or (Term 1) (Or (Term 2) (Or (Term 3) (Or (Term 4) (Or (Term 5) (Or (Term 6) (Or (Term 7) (Or (Term 8) (Term 9)))))))))
              (Kleene (Or (Term 0) (Or (Term 1) (Or (Term 2) (Or (Term 3) (Or (Term 4) (Or (Term 5) (Or (Term 6) (Or (Term 7) (Or (Term 8) (Term 9)))))))))))
            )
          )
        )

------------------------------------------------------
-- Operadores aritméticos: + | - | * | /
------------------------------------------------------
op_arit = Or 
            (Term +) 
            (Or (Term -) (Or (Term *) (Term /)))

------------------------------------------------------
-- Asignación: :=
------------------------------------------------------
asign = Concat (Term :) (Term =)

------------------------------------------------------
-- Operadores relacionales: < | > | =
------------------------------------------------------
op_rel = Or 
            (Term <) 
            (Or (Term >) (Term =))


------------------------------------------------------
-- Palabras reservadas
------------------------------------------------------
res = Or
              (Concat (Term i) (Term f))
              (Or (Concat (Term t) (Concat (Term h) (Concat (Term e) (Term n))))
                  (Or (Concat (Term e) (Concat (Term l) (Concat (Term s) (Term e))))
                      (Or (Concat (Term w) (Concat (Term h) (Concat (Term i) (Concat (Term l) (Term e)))))
                          (Concat (Term d) (Term o))))
              )

------------------------------------------------------
-- Puntuación
------------------------------------------------------
punt = Term ;

------------------------------------------------------
-- Delimitadores
------------------------------------------------------
delim = Or 
          (Term { ) (Term })

-- No funciona con:
-- delim = Or 
--          (Term { )
--          (Or (Term } ) (Or (Term ( ) (Term ) )))

-- Faltan booleanos: true, false, =, <=, not, and

-- Faltan skip, espacios, comentarios?