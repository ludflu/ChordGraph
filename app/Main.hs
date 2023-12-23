{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Graph (Graph, Vertex, graphFromEdges)

edgeList :: [(String, String, [String])]
edgeList =
  [ ("A", "A", ["E", "Cs", "Fs", "D", "F", "C"]), --
    ("B", "B", ["Fs", "Ds", "Gs", "E", "G", "D"]), --
    ("C", "C", ["G", "E", "A", "F", "Ab", "Eb"]), --
    ("D", "D", ["A", "Fs", "B", "G", "Bb", "F"]), --
    ("E", "E", ["B", "Gs", "Cs", "A", "C", "G"]), --
    ("F", "F", ["C", "A", "D", "Bb", "Db", "Ab"]), --
    ("G", "G", ["D", "B", "E", "C", "Eb", "Bb"]) --
  ]

graph :: Graph
nodeFromVertex :: Vertex -> (String, String, [String])
vertexFromKey :: String -> Maybe Vertex
(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList

main :: IO ()
main = putStrLn "Hello, Haskell!"
