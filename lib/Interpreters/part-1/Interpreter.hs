{-# LANGUAGE NoMonomorphismRestriction #-}

module Interpreter where

import Semantics

{-
    This module contains actuall interpreter.
    Semantics is divided in different classes and instances
    for better representation and decomposition.
-}

newtype R a = R{unR :: a}

instance SemanticsInt R where
    int x     = R x
    add e1 e2 = R $ unR e1 + unR e2
    mul e1 e2 = R $ unR e1 * unR e2
    neg e     = R $ -(unR e)
    eq  e1 e2 = R $ (unR e1) == (unR e2)
    leq e1 e2 = R $ (unR e1) <= (unR e2)
    geq e1 e2 = R $ (unR e1) >= (unR e2)

instance SemanticsBool R where
    bool x       = R x
    and_ e1 e2   = R $ (unR e1) && (unR e2) -- added and and or funcions additionally.
    or_ e1 e2    = R $ (unR e1) || (unR e2)
    if_ e1 e2 e3 = R $ if (unR e1) then (unR e2) else (unR e3)

instance SemanticsPair R where 
    pair e1 e2 = R $ (unR e1, unR e2)
    first e    = R $ fst (unR e)
    second e   = R $ snd (unR e)

instance SemanticsLambda R where 
    lam f     = R $ unR . f . R
    app e1 e2 = R $ (unR e1) (unR e2)

instance SemanticsFix R where
    fix f = R $ fx (unR . f . R) where fx f = f (fx f)

eval e = unR e