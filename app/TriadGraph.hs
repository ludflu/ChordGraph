module TriadGraph where

import qualified Data.Graph.Inductive as G
import qualified Data.List as DL
import qualified Data.Map as Map

type TriadNodeLabel = [String] -- the node is the set of notes in the triad

type TriadEdgeLabel = Int -- the edge is the difference in magnitude of the tone being swapped
-- out compared to the tone being swapped in

-- question: give a triad, is there a formula that will tell you if a give chord is minor or major?
notetriads :: [[String]]
notetriads =
  [ ["C", "D#", "G"],
    ["C", "D#", "G#"],
    ["C", "E", "G"],
    ["C", "E", "A"],
    ["C", "F", "G#"],
    ["C", "F", "A"],
    ["C#", "E", "G#"],
    ["C#", "E", "A"],
    ["C#", "F", "G#"],
    ["C#", "F", "A#"],
    ["C#", "F#", "A"],
    ["C#", "F#", "A#"],
    ["D", "F", "A"],
    ["D", "F", "A#"],
    ["D", "F#", "A"],
    ["D", "F#", "B"],
    ["D", "G", "A#"],
    ["D", "G", "B"],
    ["D#", "F#", "A#"],
    ["D#", "F#", "B"],
    ["D#", "G", "A#"],
    ["D#", "G#", "B"],
    ["E", "G", "B"],
    ["E", "G#", "B"]
  ]

hasEdge :: TriadNodeLabel -> TriadNodeLabel -> Bool
hasEdge triad1 triad2 =
  let sharedNotes = DL.intersect triad1 triad2
   in length sharedNotes == 2

findMates :: [TriadNodeLabel] -> TriadNodeLabel -> [TriadNodeLabel]
findMates triads triad = DL.filter (hasEdge triad) triads

nodeLookup :: Map.Map TriadNodeLabel Int
nodeLookup =
  let nodesWithIndex = zip notetriads [0 ..]
   in Map.fromList nodesWithIndex

neighborChords =
  let cfinder = findMates notetriads
   in map cfinder notetriads

makePairs :: a -> [a] -> [(a, a)]
makePairs item friends = map (\f -> (f, item)) friends

triadEdges :: [([String], [String])]
triadEdges =
  let es = map (\t -> (t, findMates notetriads t)) notetriads
      pairs = concatMap (\(triad, mates) -> makePairs triad mates) es
   in pairs

-- triadGraph :: Graph gr => gr TriadNodeLabel TriadEdgeLabel
-- triadGraph = mkGraph nodes es
--   where
--     nodes = zip [0 ..] notetriads
--     es = triadEdges
