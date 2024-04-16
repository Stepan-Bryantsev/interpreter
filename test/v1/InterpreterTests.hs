module InterpreterTests where

import Semantics
import Interpreter
import Test.HUnit

-- This section provides simple tests for all interpreter functions expressions.

-- Testing SemanticsInt
intExpr = int 1
testInt = TestCase $ assertEqual "Int test" 1 (eval intExpr)

addExpr = add (int 1) (int 2)
testAdd = TestCase $ assertEqual "Add test (1 + 2 = 3)" 3 (eval addExpr)

mulExpr = mul (int 7) (int 8)
testMul = TestCase $ assertEqual "Mul test (7 * 8 = 56)" 56 (eval mulExpr)

negExpr = neg (int 5)
testNeg = TestCase $ assertEqual "Neg test" (-5) (eval negExpr)

eqExpr1 = eq (int 5) (int 5)
testEq1 = TestCase $ assertEqual "Eq test 1 (5==5 = True)" True (eval eqExpr1)

eqExpr2 = eq (int 5) (int 6)
testEq2 = TestCase $ assertEqual "Eq test 2 (5 == 6 = False)" False (eval eqExpr2)

leqExpr1 = leq (int 5) (int 10)
testLeq1 = TestCase $ assertEqual "Leq test 1 (5 <= 10 = True)" True (eval leqExpr1)

leqExpr2 = leq (int 7) (int 6)
testLeq2 = TestCase $ assertEqual "Leq test 2 (7 <= 6 = False)" False (eval leqExpr2)

geqExpr1 = geq (int 5) (int 10)
testGeq1 = TestCase $ assertEqual "Leq test 1 (5 <= 10 = False)" False (eval geqExpr1)

geqExpr2 = geq (int 7) (int 6)
testGeq2 = TestCase $ assertEqual "Leq test 2 (7 <= 6 = True)" True (eval geqExpr2)

-- Testing SemanticsBool
boolExpr = bool True
testBool = TestCase $ assertEqual "Bool test" True (eval boolExpr)

andExpr = and_ (bool True) (bool False)
testAnd = TestCase $ assertEqual "And test" False (eval andExpr)

orExpr = or_ (bool True) (bool False)
testOr = TestCase $ assertEqual "Or test" True (eval orExpr)

ifExpr = if_ (geq (int 5) (int 10)) (int 5) (int 10)
testIf = TestCase $ assertEqual "If test" 10 (eval ifExpr)

-- Testing SemanticsPair
pairExpr = pair (int 1) (bool False)
testPair = TestCase $ assertEqual "Pair test" (1, False) (eval pairExpr)

firstExpr = first (pair (int 1) (bool False))
testFirst = TestCase $ assertEqual "First test" 1 (eval firstExpr)

secondExpr = second (pair (int 1) (bool False))
testSecond = TestCase $ assertEqual "Second test" False (eval secondExpr)

-- Testing SemanticsLambda
lambdaExpr = app (lam (\x -> add x x)) (int 2)
testLambda = TestCase $ assertEqual "Lambda test" 4 (eval lambdaExpr)

basicTestList = TestList [TestLabel "testInt" testInt,
                     TestLabel "testAdd" testAdd,         
                     TestLabel "testMul" testMul,      
                     TestLabel "testNeg" testNeg,
                     TestLabel "testEq1" testEq1,
                     TestLabel "testEq2" testEq2,
                     TestLabel "testLeq1" testLeq1,
                     TestLabel "testLeq2" testLeq2,
                     TestLabel "testGeq1" testGeq1,
                     TestLabel "testGeq2" testGeq2,
                     TestLabel "testBool" testBool,
                     TestLabel "testAnd" testAnd,
                     TestLabel "testOr" testOr,
                     TestLabel "testIf" testIf,
                     TestLabel "testPair" testPair,
                     TestLabel "testFirst" testFirst,
                     TestLabel "testSecond" testSecond,
                     TestLabel "testLambda" testLambda
                    ]