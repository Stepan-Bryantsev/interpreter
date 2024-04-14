{-# LANGUAGE NoMonomorphismRestriction #-}

module Partial where

import Control.Applicative (liftA, liftA3)

import Interpreter

{-
    This module contains partial interpreter.
    Semantics is divided in different classes and instances
    for better representation and decomposition.
-}


-- class PartialSemanticsLambda repr where 
--     lam :: (repr a -> repr b) -> repr (repr a -> repr b)
--     app :: repr (repr a -> repr b) -> repr a -> repr b

-- class PartialSemanticsFix repr where
--     fix :: repr (repr a -> repr a)  -> repr a


-- Instances of the interpreter. 

--newtype R a = R{unR :: a}

extract :: Partial repr a -> repr a
extract (Partial a _) = a

inject :: repr a -> Partial repr a 
inject a = Partial a Nothing 



data Partial repr a = Partial {dyn :: repr a, sta :: Maybe a}

instance SemanticsInt repr => SemanticsInt (Partial repr) where
    int x = Partial {dyn = int x, sta = Just x}
    add e1 e2 = Partial {dyn = add (dyn e1) (dyn e2), sta = liftA2 (+) (sta e1) (sta e2) }
    mul e1 e2 = Partial {dyn = mul (dyn e1) (dyn e2), sta = liftA2 (*) (sta e1) (sta e2) }
    neg e     = Partial {dyn = neg (dyn e),           sta = liftA (negate) (sta e) }
    eq  e1 e2 = Partial {dyn = eq  (dyn e1) (dyn e2), sta = liftA2 (==) (sta e1) (sta e2) }
    leq e1 e2 = Partial {dyn = leq (dyn e1) (dyn e2), sta = liftA2 (<=) (sta e1) (sta e2) }
    geq e1 e2 = Partial {dyn = geq (dyn e1) (dyn e2), sta = liftA2 (>=) (sta e1) (sta e2) }


class SemanticsLambda2 repr where
    lam_ :: (rep a -> rep b)     -> rep (rep a -> rep b)
    app_ :: rep (rep a -> rep b) -> rep a -> rep b
    fix_ :: rep (rep a -> rep a) -> rep a

instance repr => SemanticsInt (Partial repr) where
    {-
        How to properly implement lambda?
    -}
