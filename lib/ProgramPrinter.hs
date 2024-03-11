{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgramPrinter where

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

type VarCounter = Int -- Counter for tabulation of the expression.
type TabCounter = Int -- Counter for variables. 
newtype P a = P{unP:: VarCounter -> TabCounter -> Text}

-- Tabulation function.
tabulation :: TabCounter -> Text
tabulation n = Data.Text.replicate (n * 4) " "

-- Common case having a function with two parameters and operator in the middle. 
binFunc :: Text -> P a -> P b -> P c
binFunc op a b = P $ \h -> \t -> "(" <> unP a h t <> op <> unP b h t <> ")"

instance SemanticsInt P where
    int x     = P $ const $ const $ "int " <> pack (show x)
    add e1 e2 = binFunc " + " e1 e2
    mul e1 e2 = binFunc " * " e1 e2
    neg e     = P $ \h -> \t -> "-" <> unP e h t
    eq  e1 e2 = binFunc " == " e1 e2
    leq e1 e2 = binFunc " <= " e1 e2
    geq e1 e2 = binFunc " >= " e1 e2

instance SemanticsBool P where
    bool x       = P $ const $ const $ "bool " <> pack (show x)
    and_ e1 e2   = binFunc " && " e1 e2
    or_ e1 e2    = binFunc " || " e1 e2
    if_ e1 e2 e3 = P $ \h -> \t -> 
        "if " <> unP e1 h t <> 
        " then\n" <> tabulation (t + 1) <> unP e2 h (t + 1) <>
        "\n" <> tabulation t <> "else\n" <> tabulation (t + 1) <> unP e3 h (t + 1)

instance SemanticsPair P where 
    pair e1 e2 = binFunc ", " e1 e2
    first e    = P $ \h -> \t -> "fst " <> unP e h t
    second e   = P $ \h -> \t -> "snd " <> unP e h t

instance SemanticsLambda P where
    lam e = P $ \h -> \t -> 
       let x = "x" <> pack (show h)
       in "\\" <> x <> " -> (\n" <> tabulation (t + 1) <> 
            unP (e (P $ const $ const x)) (succ h) (t + 1) <> "\n" <> tabulation t <>")"
    app e1 e2 = P $ \h -> \t -> "(" <> unP e1 h t <> " " <> unP e2 h t <> ")"

instance SemanticsFix P where 
    fix e = P $ \h -> \t -> 
       let self = "self" <> pack (show h)
       in "(fix " <> self <> "." <> unP (e (P $ const $ const self)) (succ h) t <> ")"

printProgram e = unP e 0 0