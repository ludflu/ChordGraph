{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module MainOld where

import Data.Data (Typeable)
import Data.Graph.Inductive (Edge, Gr, Graph (mkGraph), Node, edges, neighbors)
import Data.Hashable
import Data.Int (Int)
import qualified Data.List as DL
import Data.List.Unique
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe
import Data.Modular
import qualified Data.Text.Lazy as L
import Data.Tuple
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude (Diagram, P2, center, circle, p2, position, r2, text, translate, (#))
import Tonnetz
import TriadGraph (Tone, noteMap)

type NoteGraph = Gr NodeLabel EdgeLabel

nodeLookup :: Map.Map NodeLabel Int
nodeLookup =
  let notesWithIndex = zip allNotes [0 ..]
   in Map.fromList notesWithIndex

nodeLookup' :: Map.Map Int NodeLabel
nodeLookup' =
  let notesWithIndex = zip [0 ..] allNotes
   in Map.fromList notesWithIndex

toneGraph :: Graph gr => gr NodeLabel EdgeLabel
toneGraph =
  let nodes = zip [0 ..] allNotes -- the index is required for the graph
      es = concatMap (\(x, y) -> fromMaybe [] (intervalEdges toneMatrix (x, y))) intcords
      edges = map (\(a, b) -> (nodeLookup Map.! a, nodeLookup Map.! b, 0)) es
   in mkGraph nodes edges

commonNeighbors :: NoteGraph -> Edge -> [Int]
commonNeighbors g (from, to) =
  let n1 = neighbors g from
      n2 = neighbors g to
   in DL.intersect n1 n2

makeThreeTuples :: Edge -> [Int] -> [(Int, Int, Int)]
makeThreeTuples (from, to) ns = map (\x -> (from, to, x)) ns

-- given a graph, will return all the 3-cliques
-- 1. for each edge in the graph
-- 2. for both vertices in the edge
-- 3. find the neighbor vertices in common
-- 4 for each neighbor vertex in common, record a 3-tuple of the vertices in the edge + the common neighbor
-- 5. sort each 3-tuple by value
-- 6. dedupe the list

threeClicks :: NoteGraph -> [[String]]
threeClicks g =
  let es = edges g
      ns = map (\edge -> (edge, commonNeighbors g edge)) es
      untuple (a, b, c) = [a, b, c]
      triads = concatMap (\(e, n) -> makeThreeTuples e n) ns
      triadLists = map untuple triads
      makeTones = map noteFromIndex
      orderedTriads = map (DL.sort . makeTones) triadLists
   in map showTriad $ sortUniq orderedTriads

noteFromIndex :: Int -> Int
noteFromIndex idx =
  let (cord, tone) = nodeLookup' Map.! idx
   in tone

noteFromTone :: Tone -> String
noteFromTone idx = noteMap Map.! idx

showTriad :: [Tone] -> [String]
showTriad ns = map noteFromTone ns

-- showTriad (a, b, c) =
--   let a' = nodeLookup' Map.! a -- from index to coordinate
--       b' = nodeLookup' Map.! b
--       c' = nodeLookup' Map.! c
--       n1 = getNote toneMatrix $ fst a' -- from coordinate to tone
--       n2 = getNote toneMatrix $ fst b'
--       n3 = getNote toneMatrix $ fst c'
--       tones = catMaybes [n1, n2, n3]
--    in map (\x -> noteMap Map.! (snd x)) tones

myNeighbors :: NoteGraph -> NodeLabel -> [NodeLabel]
myNeighbors tg i =
  let i' = nodeLookup Map.! i
      ns = neighbors tg i'
   in map (\x -> nodeLookup' Map.! x) ns

justNotes :: (RealFrac a, Enum a) => [NodeLabel] -> [((a, a), Integer)]
justNotes notes =
  map (\(xy, n) -> (realTuple xy, n)) notes

circleAtPoint :: ((Double, Double), Integer) -> Diagram B
circleAtPoint ((x, y), n) =
  let n' :: Tone = toMod n
      noteName = noteMap Map.! n' -- unsafe!
   in ( circle 0.75
          <> (center $ text $ noteName)
      )
        # translate (r2 (x, y))

main :: IO ()
main = mainWith field
  where
    notes = justNotes allNotes
    cs = map circleAtPoint notes
    cords = map fst notes
    points = map p2 cords
    field = position $ zip points cs
