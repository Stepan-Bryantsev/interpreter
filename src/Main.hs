{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Interpreter
import HaskellPrinter
import ProgramPrinter
import Data.Text.IO as T

-- Some demonstrations that the project is working!
-- For other cases look in the test package. 

expr1 = app (lam (\x -> (if_ (geq x (int 0)) (bool True) (bool False)))) (int 1)

main :: IO ()
main = do
       print "Run the program:"
       print (show (eval expr1))
       print "Haskel program:" 
       T.putStrLn (printHaskell expr1)
       print "Pretty program:" 
       T.putStrLn (printProgram expr1)