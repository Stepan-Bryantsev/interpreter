{-# LANGUAGE NoMonomorphismRestriction #-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StarIsType              #-}



module Abstract where

import Interpreter


data State = On | Off | AnyState deriving (Eq)
data Sign = Positive | NonPositive | Zero | Negative | NonNegative | AnySign deriving (Eq)

type family Semantics2 a :: * where
                    Semantics2 Bool           = State
                    Semantics2 Int            = Sign
                    Semantics2 (a -> b)       = Semantics2 a -> Semantics2 b
                    Semantics2 (a, b)         = (Semantics2 a, Semantics2 b)
                    Semantics2 (Either a b)   = Either (Semantics2 a) (Semantics2 b)
                    Semantics2 (A a) = A a

newtype A a = A (Semantics2 a)
--type instance Semantics2 (A a) = A a

-- evalIf :: A a -> A a -> A a
-- evalIf On On = A On

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

    if_ e1 e2 e3 = A $ case (unA e1) of 
                On -> (unA e2)
                Off -> (unA e3)
                {-
                ??? 
                AnyState -> AnyState or AnySign 
                
                How to check the type of (unA e2) and (unA e3)?

                Plus I can't return State of Sign here because the semantics is repr Bool -> repr a -> repr a -> repr a

                -}
                

    
                
