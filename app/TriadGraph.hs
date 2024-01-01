module TriadGraph where

import qualified Data.List as DL

notetriads =
  [ ["C", "D#", "G"], --
    ["C", "D#", "G#"], --
    ["C", "E", "G"], --
    ["C", "E", "A"], --
    ["C", "F", "G#"], --
    ["C", "F", "A"], --
    ["C#", "E", "G#"], --
    ["C#", "E", "A"], --
    ["C#", "F", "G#"], --
    ["C#", "F", "A#"], --
    ["C#", "F#", "A"], --
    ["C#", "F#", "A#"], --
    ["D", "F", "A"], --
    ["D", "F", "A#"],
    ["D", "F#", "A"], --
    ["D", "F#", "B"], --
    ["D", "G", "A#"],
    ["D", "G", "B"], --
    ["D#", "F#", "A#"],
    ["D#", "F#", "B"],
    ["D#", "G", "A#"],
    ["D#", "G#", "B"],
    ["E", "G", "B"],
    ["E", "G#", "B"]
  ]

hasEdge :: [String] -> [String] -> Bool
hasEdge triad1 triad2 =
  let sharedNotes = DL.intersect triad1 triad2
   in length sharedNotes == 2

--  findTriadEdges
-- for each triad

findMates :: [[String]] -> [String] -> [[String]]
findMates triads triad = DL.filter (hasEdge triad) triads

neighborChords =
  let cfinder = findMates notetriads
   in map cfinder notetriads