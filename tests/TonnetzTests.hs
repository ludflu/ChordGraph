module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testOne = testCase "test one" $ assertEqual "test one" 3 4

tonnetzUnitTests :: TestTree
tonnetzUnitTests =
  testGroup
    "tonnetz tests"
    [testOne]

main :: IO ()
main = defaultMain tonnetzUnitTests
