module Main (main) where

import AFD 

main :: IO ()
main = do
    -- Prueba sencilla para AFD
    print (acepta "1100" afdPar)
