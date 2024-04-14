{-# LANGUAGE NoMonomorphismRestriction #-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Abstract where

import Interpreter


data State = On | Off | AnyState deriving (Eq)
data Sign = Positive | NonPositive | Zero | Negative | NonNegative | AnySign deriving (Eq)

type family Semantics2 a :: * 
type instance Semantics2 bool           = State
type instance Semantics2 int            = Sign
--type instance Semantics2 (a -> b)      = Semantics2 a -> Semantics2 b
--type instance Semantics2 (pair a b)    = (Semantics2 a, Semantics2 b)
--type instance Semantics2 (Either_ a b)  = Either (Semantics2 a) (Semantics2 b)

newtype A a = A (Semantics2 a)
type instance Semantics2 (A a) = A a

unA :: A a -> Semantics2 a
unA (A x) = x

-- interpretInt :: Int -> Sign
-- interpretInt x | x > 0     = Positive
--                | x == 0    = Zero
--                | otherwise = Negative

-- instance SemanticsInt A where 
--     int x = A (interpretInt x)

--     add e1 e2 = A $ case (e1, e2) of 
--                 (Positive, Positive) -> Positive
--                 (Negative, Negative) -> Negative
--                 (Zero, Zero) -> Zero
--                 (_, _) -> AnySign

--     mul e1 e2 = A $ case (e1, e2) of 
--             (Positive, Positive) -> Positive
--             (Negative, Negative) -> Positive
--             (Positive, Negative) -> Negative
--             (Negative, Positive) -> Negative
--             (Zero, _) -> Zero
--             (_, Zero) -> Zero
--             (_, _) -> AnySign
