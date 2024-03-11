module Main where

import Test.HUnit
import qualified System.Exit as Exit

import InterpreterTests
import ProgramLengthTests
import PrintingTests

 
main :: IO ()
main = do
  runTestTT basicTestList
  runTestTT programLengthTestList
  runTestTT printingTestList
  return ()