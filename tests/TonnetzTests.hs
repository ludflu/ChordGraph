module Main where

import ChordGraph (triadGraph)
import Data.Foldable
import qualified Data.Graph.Inductive as G
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

chordNodes = G.nodes triadGraph

goodNode :: G.Node -> Bool
goodNode n =
  let es = G.out triadGraph n
   in length es == 3

allGoodNodes :: [G.Node] -> Bool
allGoodNodes ns =
  let ag = map goodNode ns
   in and ag

tonnetzUnitTests :: TestTree
tonnetzUnitTests =
  testGroup
    "tonnetz tests"
    [ testCase "All good nodes" $
        assertEqual
          "all good"
          True
          ( allGoodNodes
              chordNodes
          ),
      testCase
        "number of nodes"
        $ assertEqual "num nodes" (length chordNodes) 24
    ]

main :: IO ()
main = defaultMain tonnetzUnitTests
