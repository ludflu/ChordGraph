{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- module TriadGraph where

module Main where

import Data.Graph.Inductive (Graph (match))
import qualified Data.Graph.Inductive as G
import qualified Data.List as DL
import Data.List.Unique (sortUniq)
import qualified Data.Map as Map
import Data.Modular (ℤ, type (/))

data TriadicTransform = Leading | Relative | Parallel deriving (Show, Eq)

-- | Slide | Nebenverwandt | Hexpole
type TriadNodeLabel = [String] -- the node is the set of notes in the triad

type TriadEdgeLabel = TriadicTransform -- the edge is the difference in magnitude of the tone being swapped
-- out compared to the tone being swapped in

type TriadGraph = G.Gr TriadNodeLabel TriadEdgeLabel

-- question: give a triad, is there a formula that will tell you if a give chord is minor or major?
notetriads :: [[String]]
notetriads =
  [ ["C", "E", "G"], -- CMAJOR
    ["C#", "E", "G#"], -- C# MAJOR
    ["D", "F#", "A"], -- D MAJOR
    ["D#", "G", "A#"], -- Eb Major
    ["E", "G#", "B"], -- E Major
    ["F", "A", "C"], -- F Major
    ["F#", "A#", "C#"], -- F# Major
    ["G", "D", "B"], -- G Major
    ["G#", "C", "D#"], -- Ab Major
    ["A", "C#", "E"], -- A Major
    ["A#", "D", "F"], -- Bb Major
    ["B", "D#", "G#"], -- B Major
    ["C", "D#", "G"], -- C Minor
    ["C#", "E", "G#"], -- C # Minor
    ["D", "F", "A"], -- D Minor
    ["D#", "F#", "A#"], -- Eb Minor
    ["E", "G", "B"], -- E Minor
    ["F", "G#", "C"], -- F Minor
    ["F#", "A", "C#"], -- F# Minor
    ["G", "A#", "D"], -- G Minor
    ["G#", "B", "D#"], -- Ab Minor
    ["A", "C", "E"], -- A Minor
    ["A#", "C#", "F"], -- Bb Minor
    ["B", "D", "F#"] -- B Minor
  ]

type Tone = ℤ / 12

type Triad = (Tone, Tone, Tone)

nodeLookup :: Map.Map TriadNodeLabel Int
nodeLookup =
  let triadsWithIndex = zip notetriads [0 ..]
   in Map.fromList triadsWithIndex

nodeLookup' :: Map.Map Int TriadNodeLabel
nodeLookup' =
  let triadsWithIndex = zip [0 ..] notetriads
   in Map.fromList triadsWithIndex

noteMap :: Map.Map Tone String
noteMap =
  let notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in Map.fromList $ zip [0 .. 11] notes

noteMap' :: Map.Map String Tone
noteMap' =
  let notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in Map.fromList $ zip notes [0 .. 11]

mapTriad :: Triad -> [String]
mapTriad (a, b, c) = [noteMap Map.! a, noteMap Map.! b, noteMap Map.! c]

mapTriad' :: [String] -> Triad
mapTriad' notes =
  let (a, b, c) = (head notes, notes !! 1, notes !! 2)
   in (noteMap' Map.! a, noteMap' Map.! b, noteMap' Map.! c)

hasEdge :: TriadNodeLabel -> TriadNodeLabel -> Bool
hasEdge triad1 triad2 =
  let sharedNotes = DL.intersect triad1 triad2
   in length sharedNotes == 2

findMates :: [TriadNodeLabel] -> TriadNodeLabel -> [TriadNodeLabel]
findMates triads triad = DL.filter (hasEdge triad) triads

neighborChords :: [[TriadNodeLabel]]
neighborChords =
  let cfinder = findMates notetriads
   in map cfinder notetriads

makePairs :: a -> [a] -> [(a, a)]
makePairs item = map (\f -> (f, item))

triadEdges :: [([String], [String])]
triadEdges =
  let es = map (\t -> (t, findMates notetriads t)) notetriads
      pairs = concatMap (uncurry makePairs) es
   in pairs

makeTriadEdge :: TriadNodeLabel -> TriadNodeLabel -> (Int, Int, TriadicTransform)
makeTriadEdge from to =
  let fromIdx = nodeLookup Map.! from
      toIdx = nodeLookup Map.! to
      edgelabel = findChanged' from to
   in (fromIdx, toIdx, edgelabel)

triadGraph :: TriadGraph
triadGraph = G.mkGraph nodes edges
  where
    nodes = zip [0 ..] notetriads
    edges = map (uncurry makeTriadEdge) triadEdges

triadFinder :: Int -> TriadNodeLabel
triadFinder idx = nodeLookup' Map.! idx

findNeighbors :: TriadNodeLabel -> [TriadNodeLabel]
findNeighbors triad =
  let tidx = nodeLookup Map.! triad
      ns = sortUniq $ G.neighbors triadGraph tidx
   in map triadFinder ns

toneInterval :: Tone -> Tone -> Tone
toneInterval a b = a - b

sortThree :: Tone -> Tone -> Tone -> (Tone, Tone, Tone)
sortThree a b c =
  let sorted = DL.sort [a, b, c]
      x = head sorted
      y = sorted !! 1
      z = last sorted
   in (x, y, z)

untuple :: Triad -> [Tone]
untuple (a, b, c) = [a, b, c]

findChanged' :: [String] -> [String] -> TriadicTransform
findChanged' t1 t2 =
  let t1' = mapTriad' t1
      t2' = mapTriad' t2
      changeInterval = findChanged t1' t2'
   in describeTransform t1' t2'

findChanged :: Triad -> Triad -> Tone
findChanged t1 t2 =
  let as :: [Tone] = untuple t1
      bs :: [Tone] = untuple t2
      common :: [Tone] = DL.intersect as bs
      tnote1 = filter (`DL.notElem` common) as
      tnote2 = filter (`DL.notElem` common) bs
      n1 = head tnote1
      n2 = head tnote2
   in toneInterval n2 n1

makeTxform :: Tone -> Bool -> TriadicTransform
makeTxform delta rootDiffers
  | delta == 2 || delta == 10 = Relative
  | (delta == 1 || delta == 11) && rootDiffers = Leading
  | (delta == 1 || delta == 11) && not rootDiffers = Parallel

describeTransform :: Triad -> Triad -> TriadicTransform
describeTransform t1 t2 =
  let delta = findChanged t1 t2
      (r1, _, _) = t1
      (r2, _, _) = t2
      rootDiffers = r1 /= r2
   in makeTxform delta rootDiffers

-- isMajor' :: Tone -> Tone -> Tone -> Bool
-- isMajor' x y z =
--   let (a, b, c) = sortThree x y z
--       i1 = abs $ b - a
--       i2 = abs $ c - a
--    in (i1, i2) == (4, 7)

-- isMinor' :: Tone -> Tone -> Tone -> Bool
-- isMinor' x y z =
--   let (a, b, c) = sortThree x y z
--       i1 = abs $ b - a
--       i2 = abs $ c - a
--    in (i1, i2) == (3, 7)

-- isMajor :: String -> String -> String -> Bool
-- isMajor a b c =
--   let a' = noteMap' Map.! a
--       b' = noteMap' Map.! b
--       c' = noteMap' Map.! c
--    in isMajor' a' b' c'

-- isMinor :: String -> String -> String -> Bool
-- isMinor a b c =
--   let a' = noteMap' Map.! a
--       b' = noteMap' Map.! b
--       c' = noteMap' Map.! c
--    in isMinor' a' b' c'

third :: (a, b, c) -> c
third (_, _, c) = c

matchTriadicTransform :: TriadicTransform -> G.LEdge TriadEdgeLabel -> Bool
matchTriadicTransform tx edge = third edge == tx

makePath :: TriadicTransform -> TriadNodeLabel -> TriadNodeLabel
makePath tx origin =
  let originIdx = nodeLookup Map.! origin
      edges = G.out triadGraph originIdx
      matchingEdges = filter (matchTriadicTransform tx) edges
      matchingNodes = map (\(from, to, label) -> nodeLookup' Map.! to) matchingEdges
   in head matchingNodes

p :: TriadNodeLabel -> TriadNodeLabel
p = makePath Parallel

r :: TriadNodeLabel -> TriadNodeLabel
r = makePath Relative

l :: TriadNodeLabel -> TriadNodeLabel
l = makePath Leading

-- starting from a major chord: magical
-- starting from a minor chord: sinister
-- magical1 / sinister
lp :: TriadNodeLabel -> TriadNodeLabel
lp = l . p

-- starting from a major chord: magical
-- starting from a minor chord: sinister
-- magical2 / sinister2
pl :: TriadNodeLabel -> TriadNodeLabel
pl = p . l

-- heroic1 / uncanny
pr :: TriadNodeLabel -> TriadNodeLabel
pr = p . r

-- heroic2 / uncanny
rp :: TriadNodeLabel -> TriadNodeLabel
rp = r . p

prl :: TriadNodeLabel -> TriadNodeLabel
prl = p . r . l

slide :: TriadNodeLabel -> TriadNodeLabel
slide = l . p . r

hexapole :: TriadNodeLabel -> TriadNodeLabel
hexapole = l . p . l

-- nebenverwandt
n :: TriadNodeLabel -> TriadNodeLabel
n = r . l . p

type TriadTraversal = TriadNodeLabel -> TriadNodeLabel

findChordProgression :: TriadNodeLabel -> [TriadTraversal] -> [TriadNodeLabel]
findChordProgression start [] = []
findChordProgression start (hd : tl) =
  let next = hd start
   in next : findChordProgression next tl

main :: IO ()
main =
  let cmajor = ["C", "E", "G"]
      path = [p, r, l, slide, lp, pl, pr, rp, hexapole]
      progression = cmajor : findChordProgression cmajor path
   in print progression
