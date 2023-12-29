{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Data (Typeable)
-- import Data.Function (on)

-- import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Graph.Inductive (Edge, Graph (mkGraph), Node, edges, neighbors)
import Data.Hashable
import Data.Int (Int)
import qualified Data.List as DL
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Tuple
import Data.Word
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

type NodeLabel = (Int, Int)

type EdgeLabel = Int

netlines :: [[Int]]
netlines =
  [ [0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0],
    [3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1, 8],
    [11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11],
    [2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0, 7],
    [10, 5, 0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10],
    [1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6],
    [9, 4, 11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9],
    [0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5],
    [8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1, 8],
    [11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4],
    [7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0, 7],
    [10, 5, 0, 7, 2, 9, 4, 11, 6, 1, 8, 3],
    [6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6],
    [9, 4, 11, 6, 1, 8, 3, 10, 5, 0, 7, 2],
    [5, 0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5],
    [8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1],
    [4, 11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4],
    [7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0],
    [3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1, 8, 3],
    [6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11],
    [2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0, 7, 2],
    [5, 0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10],
    [1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1],
    [4, 11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9],
    [0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0]
  ]

noteMap :: Map.Map Int String
noteMap =
  let notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
   in Map.fromList $ zip [0 .. 11] notes

evens :: [a] -> [a]
evens xs = [x | (x, index) <- zip xs [0 ..], even index]

odds :: [a] -> [a]
odds xs = [x | (x, index) <- zip xs [0 ..], odd index]

alternatePad = DL.intersperse 255

surroundPad xs = 255 : xs ++ [255]

interleave xs ys = concat (DL.transpose [xs, ys])

fifth :: (Int, Int) -> (Int, Int)
fifth (x, y) = (x + 2, y)

fifth' :: (Int, Int) -> (Int, Int)
fifth' (x, y) = (x - 2, y)

majorThird :: (Int, Int) -> (Int, Int)
majorThird (x, y) = (x - 1, y - 1)

majorThird' :: (Int, Int) -> (Int, Int)
majorThird' (x, y) = (x + 1, y + 1)

minorThird :: (Int, Int) -> (Int, Int)
minorThird (x, y) = (x - 1, y + 1)

minorThird' :: (Int, Int) -> (Int, Int)
minorThird' (x, y) = (x + 1, y - 1)

mkIndex :: (Int, Int) -> Int
mkIndex (x, y) = y * 25 + x

toneMatrix :: M.Matrix Int
toneMatrix =
  let es = map alternatePad $ evens netlines
      os = map (surroundPad . alternatePad) $ odds netlines
      matdata = interleave es os
   in M.transpose $ M.fromLists matdata

getNote :: M.Matrix Int -> (Int, Int) -> Maybe ((Int, Int), Int)
getNote toneMat (x, y) =
  let f = M.safeGet x y toneMat
   in if f == Just 255 then Nothing else fmap (\i -> ((x, y), i)) f

intervalEdges :: M.Matrix Int -> (Int, Int) -> Maybe [((Int, Int), (Int, Int))]
intervalEdges toneMat (x, y) =
  do
    (loc, focus) <- getNote toneMat (x, y)
    let raiseFifth = getNote toneMat (fifth (x, y))
        raiseMajorThird = getNote toneMat (majorThird (x, y))
        lowerMinorThird = getNote toneMat (minorThird (x, y))
        intervals = catMaybes [raiseFifth, raiseMajorThird, lowerMinorThird]
     in return $ map (\(loc', note) -> (loc, loc')) intervals

allNotes :: [((Int, Int), Int)]
allNotes =
  let notefetcher = getNote toneMatrix
   in mapMaybe notefetcher intcords

coordinates :: (Fractional a, Enum a) => [(a, a)]
coordinates = concat [[(i, j) | j <- [1.0 .. 25.0]] | i <- [1.0 .. 25.0]]

floorTuple :: (RealFrac a, Enum a) => (a, a) -> (Int, Int)
floorTuple (x, y) = (floor x, floor y)

realTuple :: (RealFrac a, Enum a) => (Int, Int) -> (a, a)
realTuple (x, y) = (fromIntegral x, fromIntegral y)

intcords :: [(Int, Int)]
intcords = map floorTuple coordinates

justNotes :: (RealFrac a, Enum a) => [((a, a), Int)]
justNotes =
  map (\(xy, n) -> (realTuple xy, n)) allNotes

nodeLookup :: Map.Map (Int, Int) Int
nodeLookup =
  let notes = map fst allNotes
      notesWithIndex = zip notes [0 ..]
   in Map.fromList notesWithIndex

nodeLookup' :: Map.Map Int (Int, Int)
nodeLookup' =
  let notes = map fst allNotes
      notesWithIndex = zip [0 ..] notes
   in Map.fromList notesWithIndex

toneGraph :: Graph gr => gr NodeLabel EdgeLabel
toneGraph =
  let nodes :: [(Int, (Int, Int))] = zip [0 ..] (map fst allNotes) -- the index is required for the graph
      es = concatMap (\(x, y) -> fromMaybe [] (intervalEdges toneMatrix (x, y))) intcords
      edges = map (\(a, b) -> (nodeLookup Map.! a, nodeLookup Map.! b, 0)) es
   in mkGraph nodes edges

commonNeighbors :: Graph gr => gr (Int, Int) (Int, Int) -> Edge -> [Node]
commonNeighbors g (from, to) =
  let n1 = neighbors g from
      n2 = neighbors g to
   in DL.intersect n1 n2

-- given a graph, will return all the 3-cliques
-- threeClicks :: Graph gr => gr (Int, Int) (Int, Int) -> [(Int, Int, Int)]
-- threeClicks g =
--   let es = edges g
--       ns = map (\(x, y) -> (commonNeighbors g (x, y), (x, y))) es
--    in []

-- 1. for each edge in the graph
-- 2. for both vertices in the edge
-- 3. find the neighbor vertices in common
-- 4 for each neighbor vertex in common, record a 3-tuple of the vertices in the edge + the common neighbor
-- 5. sort each 3-tuple by value
-- 6. dedupe the list

myNeighbors :: Graph gr => gr NodeLabel EdgeLabel -> NodeLabel -> [NodeLabel]
myNeighbors tg i =
  let i' = nodeLookup Map.! i
      ns = neighbors tg i'
   in map (\x -> nodeLookup' Map.! x) ns

points =
  let pts = map fst justNotes
   in map p2 pts

circleAtPoint :: ((Double, Double), Int) -> Diagram B
circleAtPoint ((x, y), n) =
  let noteName = noteMap Map.! n -- unsafe!
   in ( circle 0.75
          <> (center $ text $ noteName)
      )
        # translate (r2 (x, y))

cs = map circleAtPoint justNotes

field = position $ zip points cs

main :: IO ()
main = mainWith field
