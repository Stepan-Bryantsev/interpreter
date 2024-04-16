{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Compiler where

{-
    Here is implementation of the compiler module using haskell templates. 
-}

import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as Syntax
import GHC.Exts

import Semantics
import Helpers

newtype C a = C ExpQ
unC :: C a -> ExpQ
unC (C x) = x

liftC0 :: Syntax.Lift t => t -> C a
liftC0 x                = C [| x |]

liftC1 :: Q Exp -> C a -> C b
liftC1 g (C a)          = C $ do
                            g' <- g
                            a' <- a
                            return (AppE g' a')

liftC2 :: Q Exp -> C a -> C b -> C c
liftC2 g (C a) (C b)     = C $ do
                            g' <- g
                            a' <- a
                            b' <- b
                            return (UInfixE a' g' b')


instance SemanticsInt C where
    int = liftC0
    add = liftC2 [| (+) |]
    mul = liftC2 [| (*) |]
    neg = liftC1 [| negate |]
    eq  = liftC2 [| (==) |]
    leq = liftC2 [| (<=) |]
    geq = liftC2 [| (>=) |]

instance SemanticsBool C where
    bool = liftC0
    and_ = liftC2 [| (&&) |]
    or_  = liftC2 [| (&&) |]
    if_ e1 e2 e3 = C [| if $(unC e1) then $(unC e2) else $(unC e3) |]


instance SemanticsPair C where 
    pair e1 e2 = C $ [| ($(unC e1), $(unC e1)) |]
    first      = liftC1 [| fst |]
    second     = liftC1 [| snd |]

instance SemanticsLambda C where 
    lam f = C $ do
        x <- Syntax.newName "x"
        body <- unC (f (C (varE x)))
        return $ Syntax.LamE [Syntax.VarP x] body

    app f x = C $ [| $(unC f) $(unC x) |]