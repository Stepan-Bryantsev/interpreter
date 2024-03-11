{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PrintingTests where

import Interpreter
import HaskellPrinter
import ProgramPrinter
import Test.HUnit

import Data.Text

default (Text)
{-
    This module contains program printing tests.
    Program printing contains some tabulations ans new lines for better visualisation, 
    so it might loog a bit ugly if its not actually printed. 
-}

addExpr = add (int 1) (int 2)
expectedHaskel = "(1 + 2)"
expectedProgram = "(int 1 + int 2)"

testHaskelAdd = TestCase $ assertEqual "Add print haskel test" expectedHaskel (printHaskell addExpr)
testProgramAdd = TestCase $ assertEqual "Add print program test" expectedProgram (printProgram addExpr)

complexExpr = lam (\x -> (if_ (geq x (int 0)) (bool True) (bool False)))
expectedHaskel1 = "(\\x0 -> if (x0 >= 0) then True else False)"
expectedProgram1 = "\\x0 -> (\n    if (x0 >= int 0) then\n        bool True\n    else\n        bool False\n)"

testHaskel = TestCase $ assertEqual "Print haskel test" expectedHaskel1 (printHaskell complexExpr)
testProgram = TestCase $ assertEqual "Print program test" expectedProgram1 (printProgram complexExpr)

printingTestList = TestList [TestLabel "testHaskelAdd" testHaskelAdd,
                            TestLabel "testProgramAdd" testProgramAdd,
                            TestLabel "testHaskel" testHaskel,
                            TestLabel "testProgram" testProgram
                    ]