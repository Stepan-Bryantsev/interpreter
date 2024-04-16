{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StarIsType #-}

{-
    Implementation of abstract interpreter here. 
-}

module Abstract where

import Semantics
import ExtendedSemantics


data State = On | Off | AnyState deriving (Eq)
data Sign = Positive | NonPositive | Zero | Negative | NonNegative | AnySign deriving (Eq, Show)

{-
    I suppose that the NonPositive and NonNegative parts are unnecessary. 
    There no cases where evaluation could return NonPositive/NonNegative,
    because the basic case for int return only Positive, Negative and Zero
-}

type family Semantics2 a :: * where
                    Semantics2 Bool           = State
                    Semantics2 Int            = Sign
                    Semantics2 (a -> b)       = Semantics2 a -> Semantics2 b
                    Semantics2 (a, b)         = (Semantics2 a, Semantics2 b)
                    Semantics2 (A a) = A a

newtype A a = A (Semantics2 a)

unA :: A a -> Semantics2 a
unA (A x) = x

instance SemanticsInt A where 
    int x = A (if (x > 0) then Positive else (if (x < 0) then Negative else Zero))

    add e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (Positive, Positive) -> Positive
                (Negative, Negative) -> Negative
                (Zero, Zero) -> Zero
                (_, _) -> AnySign

    mul e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (Positive, Positive) -> Positive
                (Negative, Negative) -> Positive
                (Positive, Negative) -> Negative
                (Negative, Positive) -> Negative
                (Zero, _) -> Zero
                (_, Zero) -> Zero
                (_, _) -> AnySign

    neg e = A $ case (unA e) of 
                Positive -> Negative
                Negative -> Positive
                NonNegative -> NonPositive
                NonPositive -> NonNegative
                Zero -> Zero
                AnySign -> AnySign

    eq e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (Positive, Negative) -> Off
                (Negative, Positive) -> Off
                (Zero, Zero) -> On
                (AnySign, AnySign) -> AnyState
                (_, _) -> AnyState

    leq e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (Negative, Positive) -> On
                (Positive, Negative) -> Off
                (Zero, Positive) -> On
                (Negative, Zero) -> On
                (Positive, Zero) -> Off
                (Zero, Negative) -> Off
                (Zero, Zero) -> On
                (AnySign, AnySign) -> AnyState
                (_, _) -> AnyState

    geq e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (Negative, Positive) -> Off
                (Positive, Negative) -> On
                (Zero, Positive) -> Off
                (Negative, Zero) -> Off
                (Positive, Zero) -> On
                (Zero, Negative) -> On
                (Zero, Zero) -> On
                (AnySign, AnySign) -> AnyState
                (_, _) -> AnyState

instance SemanticsBool A where 
    bool x = A (if x then On else Off)
    and_ e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (On, On) -> On
                (Off, _) -> Off
                (_, Off) -> Off
                (_, _) -> AnyState

    and_ e1 e2 = A $ case ((unA e1), (unA e2)) of 
                (On, _) -> On
                (_, On) -> On
                (Off, Off) -> Off
                (_, _) -> AnyState
                


{- 
    I did not find a way to support both State and Sign in then/else. 
    That why changed the semantics to work only with integers. 
-}
class AbstractSemanticsIf repr where
    if__ :: repr Bool -> repr Int -> repr Int -> repr Int

instance AbstractSemanticsIf A where 
    if__ e1 e2 e3 = A $ case (unA e1) of 
                    On -> (unA e2)
                    Off -> (unA e3)
                    -- if both branches has the same return value - then return it, else - AnySign
                    AnyState -> if (unA e2) == (unA e3) then (unA e2) else AnySign

-- Using extended semantics here 
instance ExtendedSemanticsLambda A where 
    lambda2 f      = A (\x -> f (A (unA x)))
    apply2 (A f) x = f x