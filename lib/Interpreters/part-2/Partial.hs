{-# LANGUAGE NoMonomorphismRestriction #-}

module Partial where

import Control.Applicative (liftA, liftA3)

import Semantics
import ExtendedSemantics
import Helpers

{-
    This module contains partial interpreter.
-}

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

instance SemanticsBool repr => SemanticsBool (Partial repr) where
    bool x       = Partial {dyn = bool x, sta = Just x}
    and_ e1 e2   = Partial {dyn = and_ (dyn e1) (dyn e2),         sta = liftA2 (&&) (sta e1) (sta e2)}
    or_ e1 e2    = Partial {dyn = or_ (dyn e1) (dyn e2),          sta = liftA2 (||) (sta e1) (sta e2)}
    if_ e1 e2 e3 = Partial {dyn = if_ (dyn e1) (dyn e2) (dyn e3), sta = liftA3 (if__) (sta e1) (sta e2) (sta e3)}

instance SemanticsPair repr => SemanticsPair (Partial repr) where
    pair e1 e2 = Partial {dyn = pair (dyn e1) (dyn e2), sta = liftA2 (,) (sta e1) (sta e2)}
    first e    = Partial {dyn = first (dyn e), sta = liftA fst (sta e)}
    second e   = Partial {dyn = second (dyn e), sta = liftA snd (sta e)}


{-
Got some troubles implementing lambda. Probably it should look like this:
    dyn = \x -> dyn . f . extract 
    sta = Nothing 

    Static part for lambda is nothing. 
-}