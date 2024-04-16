module Main where

import Test.HUnit
import qualified System.Exit as Exit

import InterpreterTests
import ProgramLengthTests
import PrintingTests
import AbstractTests
 
main :: IO ()
main = do
  runTestTT basicTestList
  runTestTT programLengthTestList
  runTestTT printingTestList
  runTestTT abstractTestList
  
  return ()