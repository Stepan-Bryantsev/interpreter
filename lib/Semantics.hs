{-# LANGUAGE NoMonomorphismRestriction #-}

module Semantics where

{-
    This module contains actuall interpreter.
    Semantics is divided in different classes and instances
    for better representation and decomposition.
-}

class SemanticsInt repr where
    int :: Int -> repr Int      
    add :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int
    neg :: repr Int -> repr Int
    eq  :: repr Int -> repr Int -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    geq :: repr Int -> repr Int -> repr Bool

class SemanticsBool repr where
    bool :: Bool -> repr Bool      
    and_ :: repr Bool -> repr Bool -> repr Bool
    or_ :: repr Bool -> repr Bool -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class SemanticsPair repr where 
    pair  :: repr a -> repr b -> repr (a, b)
    first :: repr (a, b) -> repr a
    second :: repr (a, b) -> repr b

class SemanticsLambda repr where 
    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b

class SemanticsFix repr where
    fix :: (repr a -> repr a)  -> repr a