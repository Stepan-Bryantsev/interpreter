module AdvancedTests where

import Semantics
import Interpreter
import Test.HUnit

intExpr = int 1
testInt = TestCase $ assertEqual "Int test" 1 (eval intExpr)

addExpr = add (int 1) (int 2)
testAdd = TestCase $ assertEqual "Add test (1 + 2 = 3)" 3 (eval addExpr)

basicTestList = TestList [TestLabel "testInt" testInt,
                     TestLabel "testAdd" testAdd
                    ]