{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellPrinter where

import Interpreter
import Data.Text

-- Using text instead of String
default (Text)

type VarCounter = Int -- Variable counter
newtype S a = S{unS:: VarCounter -> Text}

-- Common case having a function with two parameters and operator in the middle. 
binFunc :: Text -> S a -> S b -> S c
binFunc op a b = S $ \h -> "(" <> unS a h <> op <> unS b h <> ")"

instance SemanticsInt S where
    int x     = S $ const $ pack (show x)
    add e1 e2 = binFunc " + " e1 e2 
    mul e1 e2 = binFunc " * " e1 e2
    neg e     = S $ \h -> "-" <> unS e h
    eq  e1 e2 = binFunc " == " e1 e2
    leq e1 e2 = binFunc " <= " e1 e2
    geq e1 e2 = binFunc " >= " e1 e2

instance SemanticsBool S where
    bool x       = S $ const $ pack (show x)
    and_ e1 e2   = binFunc " && " e1 e2
    or_ e1 e2    = binFunc " || " e1 e2
    if_ e1 e2 e3 = S $ \h -> "if " <> unS e1 h <> " then " <> unS e2 h <> " else " <> unS e3 h

instance SemanticsPair S where 
    pair e1 e2 = binFunc " + " e1 e2
    first e    = S $ \h -> "fst " <> unS e h
    second e   = S $ \h -> "snd " <> unS e h

instance SemanticsLambda S where
    lam e = S $ \h -> 
       let x = "x" <> pack (show h)
       in "(\\" <> x <> " -> " <> 
            unS (e (S $ const x)) (succ h) <> ")"
    app e1 e2 = S $ \h -> "(" <> unS e1 h <> " " <> unS e2 h <> ")"

instance SemanticsFix S where 
    fix e = S $ \h -> 
       let self = "self" <> pack (show h)
       in "(fix " <> self <> "." <> unS (e (S $ const self)) (succ h) <> ")"

printHaskell e = unS e 0