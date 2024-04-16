module ProgramLengthTests where

import Semantics
import LengthCalculator
import Test.HUnit

-- This section provides tests for program length conputation. 

--       1    +    1  + 1  +  1   +    1     +     1           = 6
expr1 = lam (\x -> if_ (eq x (int 0)) (bool True) (bool False))

testLength1 = TestCase $ assertEqual "Lenth test (expected length 6)" 6 (programLength expr1)

--      1  + 7  +  1     = 8  
expr2 = app expr1 (int 1)

testLength2 = TestCase $ assertEqual "Lenth test (expected length 8)" 8 (programLength expr2)

programLengthTestList = TestList [TestLabel "testLength1" testLength1,
                                  TestLabel "testLength2" testLength2
                                 ]