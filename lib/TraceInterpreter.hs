{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module TraceInterpreter where

import Interpreter
import Data.Text

-- Using text instead of String
default (Text)

{-
    This module contains the program printing interpreter.  
    It was now quite clear for me how shoul it look,
    so I've created some pseudo- representation of the target program.

    Added some tabulations for if and lambda.
-}

type Level = Int -- Counter for level of the expression.
type VarCounter = Int -- Counter for variables. 
newtype T a = T { unT :: Level -> VarCounter -> Text }


--Show Level function.
showLevel :: Level -> Text
showLevel l = Data.Text.replicate (l * 4) " " <> "Level " <> pack (show l) <> ": "

-- Common case having a function with two parameters and operator in the middle. 
binFunc :: Text -> T a -> T b -> T c
binFunc op a b = T $ \l -> \h -> showLevel l <> "Eval op " <> op <> "\n" <> unT a (l + 1) h <> unT b (l + 1) h

instance SemanticsInt T where
    int x     = T $ \l -> \h -> (showLevel l) <> "int " <> pack (show x) <> "\n"
    add e1 e2 = binFunc " + " e1 e2
    mul e1 e2 = binFunc " * " e1 e2
    neg e     = T $ \l -> \h -> (showLevel l) <> "Eval negation " <> "\n" <> unT e (l + 1) h
    eq  e1 e2 = binFunc " == " e1 e2
    leq e1 e2 = binFunc " <= " e1 e2
    geq e1 e2 = binFunc " >= " e1 e2

instance SemanticsBool T where
    bool x       = T $ \l -> \h -> (showLevel l) <> "bool " <> pack (show x) <> "\n"
    and_ e1 e2   = binFunc " && " e1 e2
    or_ e1 e2    = binFunc " || " e1 e2
    if_ e1 e2 e3 = T $ \l -> \h -> (showLevel l) <> "If evaluation\n" <> 
                                    unT e1 (l + 1) h <>
                                    unT e2 (l + 1) h <>
                                    unT e3 (l + 1) h
                                    

instance SemanticsPair T where 
    pair e1 e2 = binFunc ", " e1 e2
    first e    = T $ \l -> \h -> (showLevel l) <> "Eval fst for pair " <> unT e (l + 1) h
    second e   = T $ \l -> \h -> (showLevel l) <> "Eval snd for pair" <> unT e (l + 1) h

instance SemanticsLambda T where
    lam e = T $ \l -> \h -> (showLevel l) <> "Lambda eval, init var: x" <> pack (show h) <> "\n" <>
                            unT (e (T $ const $ const (showLevel l <> "Use var x" <> pack (show h) <> "\n"))) (l + 1) (h + 1)
    app e1 e2 = T $ \l -> \h -> (showLevel l) <> "App eval\n" <> unT e1 (l + 1) h <> unT e2 (l + 1) h

-- instance SemanticsFix P where 
--     fix e = P $ \h -> \t -> 
--        let self = "self" <> pack (show h)
--        in "(fix " <> self <> "." <> unP (e (P $ const $ const self)) (succ h) t <> ")"

traceProgram e = unT e 0 0