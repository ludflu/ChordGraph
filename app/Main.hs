{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Data (Typeable)
import Data.Function (on)
import Data.Graph.Inductive (Graph (mkGraph))
import Data.Hashable
import Data.Int (Int)
import qualified Data.List as DL
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Tuple
import Data.Word
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

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
   in M.fromLists matdata

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

-- allNotes :: [(Int, L.Text)]
-- allNotes =
--   let notefetcher = getNote toneMatrix
--       ns = mapMaybe (notefetcher) coordinates
--    in map (\(a, b) -> (mkIndex a, L.pack $ show b)) ns

-- allEdges :: [(Int, Int, L.Text)]
-- allEdges =
--   let es = concatMap (\(x, y) -> fromMaybe [] (intervalEdges toneMatrix (x, y))) coordinates
--    in map (\(a, b) -> (mkIndex a, mkIndex b, "255")) es

coordinates :: (Fractional a, Enum a) => [(a, a)]
coordinates = concat [[(i, j) | j <- [1.0 .. 25.0]] | i <- [1.0 .. 25.0]]

floorTuple :: (RealFrac a, Enum a) => (a, a) -> (Int, Int)
floorTuple (x, y) = (floor x, floor y)

realTuple :: (RealFrac a, Enum a) => (Int, Int) -> (a, a)
realTuple (x, y) = (fromIntegral x, fromIntegral y)

intcords :: [(Int, Int)]
intcords = map floorTuple coordinates

justNotes :: (RealFrac a, Enum a) => [(a, a)]
justNotes =
  let ns = map fst allNotes
   in map realTuple ns

points = map p2 justNotes

circleAtPoint :: (Double, Double) -> Diagram B
circleAtPoint (x, y) = circle 1 # fc blue # translate (r2 (x, y))

cs = map circleAtPoint justNotes

field = position $ zip points cs

main :: IO ()
main = mainWith field
