{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- module TriadGraph where

module Main where

import qualified Data.Graph.Inductive as G
import qualified Data.List as DL
import qualified Data.Map as Map
import Data.Modular (ℤ, type (/))

data TriadicTransform = Leading | Relative | Parallel deriving (Show)

-- | Slide | Nebenverwandt | Hexpole
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

type Tone = ℤ / 12

type Triad = (Tone, Tone, Tone)

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
  let (a, b, c) = (notes !! 0, notes !! 1, notes !! 2)
   in (noteMap' Map.! a, noteMap' Map.! b, noteMap' Map.! c)

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

neighborChords :: [[TriadNodeLabel]]
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

noteC :: Tone
noteC = 0

noteA :: Tone
noteA = 9

noteE :: Tone
noteE = 4

noteF :: Tone
noteF = 5

toneInterval :: Tone -> Tone -> Tone
toneInterval a b = a - b

sortThree :: Tone -> Tone -> Tone -> (Tone, Tone, Tone)
sortThree a b c =
  let sorted = DL.sort [a, b, c]
      x = head sorted
      y = head $ drop 1 sorted
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
      tnote1 = filter (\n -> n `DL.notElem` common) as
      tnote2 = filter (\n -> n `DL.notElem` common) bs
      n1 = head tnote1
      n2 = head tnote2
   in toneInterval n2 n1

makeTxform :: Tone -> Bool -> TriadicTransform
makeTxform delta rootDiffers
  | delta == 2 || delta == 10 = Relative
  | delta == 1 || delta == 11 && rootDiffers = Leading
  | delta == 1 || delta == 11 && not rootDiffers = Parallel

describeTransform :: Triad -> Triad -> TriadicTransform
describeTransform t1 t2 =
  let delta = findChanged t1 t2
      (r1, _, _) = t1
      (r2, _, _) = t2
      rootDiffers = r1 /= r2
   in makeTxform delta rootDiffers

isMajor' :: Tone -> Tone -> Tone -> Bool
isMajor' x y z =
  let (a, b, c) = sortThree x y z
      i1 = abs $ b - a
      i2 = abs $ c - a
   in (i1, i2) == (4, 7)

isMinor' :: Tone -> Tone -> Tone -> Bool
isMinor' x y z =
  let (a, b, c) = sortThree x y z
      i1 = abs $ b - a
      i2 = abs $ c - a
   in (i1, i2) == (3, 7)

isMajor :: String -> String -> String -> Bool
isMajor a b c =
  let a' = noteMap' Map.! a
      b' = noteMap' Map.! b
      c' = noteMap' Map.! c
   in isMajor' a' b' c'

isMinor :: String -> String -> String -> Bool
isMinor a b c =
  let a' = noteMap' Map.! a
      b' = noteMap' Map.! b
      c' = noteMap' Map.! c
   in isMinor' a' b' c'

main :: IO ()
main = print neighborChords