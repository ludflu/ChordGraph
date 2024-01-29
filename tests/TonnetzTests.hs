module Main where

import ChordGraph (TriadicTransform (Leading, Parallel, Relative), triadGraph)
import Data.Foldable
import qualified Data.Graph.Inductive as G
import Data.List.Unique (sortUniq)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

chordNodes = G.nodes triadGraph

third :: (a, b, c) -> c
third (a, b, c) = c

-- a node is good if it has exactly three neighbors and the edges are labeled correctly
goodNode :: G.Node -> Bool
goodNode n =
  let es = G.out triadGraph n
      lbls = sortUniq $ map third es
   in length es == 3 && lbls == [Leading, Relative, Parallel]

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
