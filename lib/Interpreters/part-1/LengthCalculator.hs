{-# LANGUAGE NoMonomorphismRestriction #-}

module LengthCalculator where

import Semantics


{-
    This module contains calculation for program length.
    Each expression has length 1. 
-}

newtype L a = L{unL::Int}
instance SemanticsInt L where
    int x     = L 1
    add e1 e2 = L $ unL e1 + unL e2 + 1
    mul e1 e2 = L $ unL e1 + unL e2 + 1
    neg e     = L $ unL e + 1
    eq  e1 e2 = L $ unL e1 + unL e2 + 1
    leq e1 e2 = L $ unL e1 + unL e2 + 1
    geq e1 e2 = L $ unL e1 + unL e2 + 1

instance SemanticsBool L where
    bool x       = L 1
    and_ e1 e2   = L $ unL e1 + unL e2 + 1
    or_ e1 e2    = L $ unL e1 + unL e2 + 1
    if_ e1 e2 e3 = L $ unL e1 + unL e2 + unL e3 + 1

instance SemanticsPair L where 
    pair e1 e2 = L $ unL e1 + unL e2 + 1
    first e    = L $ unL e + 1
    second e   = L $ unL e + 1

instance SemanticsLambda L where 
    lam f     = L $ unL (f (L $ 1))
    app e1 e2 = L $ unL e1 + unL e2 + 1

instance SemanticsFix L where
    fix e = L $ unL (e (L 0)) + 1

programLength e = unL e
