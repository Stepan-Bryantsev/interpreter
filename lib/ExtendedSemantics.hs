{-# LANGUAGE NoMonomorphismRestriction #-}

module ExtendedSemantics where

class ExtendedSemanticsLambda repr where 
    lambda2 :: (repr a -> repr b)      -> repr (repr a -> repr b)
    apply2  :: repr (repr a -> repr b) -> repr a -> repr b

class ExtendedSemanticsFix repr where 
    fix2    :: repr (repr a -> repr a) -> repr a