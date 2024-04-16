module AbstractTests where

import Semantics
import Interpreter
import Abstract
import ExtendedSemantics

import Test.HUnit

intExpr = neg (mul (int 1) (int 1))
testInt = TestCase $ assertEqual "Int abstract test" Negative (unA intExpr)

-- addExpr = add (int 1) (int 2)
-- testAdd = TestCase $ assertEqual "Add test (1 + 2 = 3)" 3 (eval addExpr)

abstractTestList = TestList [TestLabel "testInt" testInt
                     --TestLabel "testAdd" testAdd
                    ]