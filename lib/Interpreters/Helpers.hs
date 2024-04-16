module Helpers (Liftable1(pull), if__) where

-- Useful helper class
class Applicative f => Liftable1 f where
  pull :: (f a -> f b) -> f (a -> b)

-- A function that does (the equivalent of) if
-- This works because Haskell is lazy, this wouldn't work in CBV
if__ :: Bool -> a -> a -> a
if__ cond tb eb = if cond then tb else eb
