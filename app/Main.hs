{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function (on)
import Data.Graph.Inductive
import Data.Graph.Inductive (Graph (mkGraph))
import Data.GraphViz
import Data.GraphViz (blankParams, nonClusteredParams)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.Hashable
import Data.Int (Int)
import qualified Data.List as DL
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Tuple
import Data.Word
import WriteRunDot

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

coordinates :: [(Int, Int)]
coordinates = concat [[(i, j) | j <- [1 .. 25]] | i <- [1 .. 25]]

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

allNotes :: [(Int, L.Text)]
allNotes =
  let notefetcher = getNote toneMatrix
      ns = mapMaybe (notefetcher) coordinates
   in map (\(a, b) -> (mkIndex a, L.pack $ show b)) ns

allEdges :: [(Int, Int, L.Text)]
allEdges =
  let es = concatMap (\(x, y) -> fromMaybe [] (intervalEdges toneMatrix (x, y))) coordinates
   in map (\(a, b) -> (mkIndex a, mkIndex b, "255")) es

toneGraph :: Gr L.Text L.Text
toneGraph =
  mkGraph allNotes allEdges

ex1 :: Gr L.Text L.Text
ex1 =
  mkGraph
    [ (1, "A"),
      (2, "B"),
      (3, "C"),
      (4, "D"),
      (5, "E"),
      (6, "F"),
      (7, "G")
    ]
    [(1, 3, "edge label")]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params =
  nonClusteredParams
    { globalAttributes = ga,
      fmtNode = fn,
      fmtEdge = fe
    }
  where
    fn (_, l) = [textLabel l]
    fe (_, _, l) = [textLabel l]

    ga =
      [ GraphAttrs
          [ RankDir FromLeft,
            BgColor [toWColor White]
          ],
        NodeAttrs
          [ shape BoxShape,
            FillColor (myColorCL 2),
            style filled
          ]
      ]

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
myColorCL :: Word8 -> ColorList
myColorCL n
  | n == 1 = c $ (RGB 127 108 138)
  | n == 2 = c $ (RGB 175 177 112)
  | n == 3 = c $ (RGB 226 206 179)
  | n == 4 = c $ (RGB 172 126 100)
  where
    c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

main :: IO ()
main =
  let dotGraph = graphToDot ex1Params toneGraph
   in do writeFile "output.dot" (L.unpack $ printDotGraph dotGraph)

-- main :: IO ()
-- main = doDots [("toneGraph", graphToDot ex1Params toneGraph)]

--  doDots [("ex1", graphToDot ex1Params ex1)]

--   doDots
--     [ ("ex3", ex3)
--     ]