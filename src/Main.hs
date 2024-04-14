{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE QuasiQuotes #-}


module Main where

import Interpreter
import HaskellPrinter
import ProgramPrinter
import Compiler
import TraceInterpreter
import Partial
import Abstract

import Data.Text.IO as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr

-- Some demonstrations that the project is working!
-- For other cases look in the test package. 

expr1 = app (lam (\x -> (if_ (geq x (int 0)) (bool True) (bool False)))) (int 1)

expr2 = unC (if_ (eq (int 1) (int 2)) (int 100) (neg (int 100)))

expr3 = add (add (int 1) (int 2)) (int 3)

exprTrace = if_ (bool True) (int 1) (int 2)

testPartial = add (int 1) (int 2)
testDyn = inject testPartial
testSta = extract testDyn

testA = add (int 1) (int 2)


main :: IO ()
main = do
       print "Run the program:"
       print (show (eval expr1))
       print "Haskel program:" 
       T.putStrLn (printHaskell expr1)
       print "Pretty program:" 
       T.putStrLn (printProgram expr1)

       print "Trace program:" 
       T.putStrLn (traceProgram exprTrace)

       tmp <- runQ expr2
       print (pprint tmp)
       print (tmp)

       print $(unC (if_ (eq (int 1) (int 2)) (int 100) (neg (int 100))))

       print (show ((unA (int 1) == Negative)))

