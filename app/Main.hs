{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.Hashable
import Data.List as DL
import qualified Data.Text.Lazy as L
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

interleave xs ys = concat (transpose [xs, ys])

padded =
  let es = map alternatePad $ evens netlines
      os = map (surroundPad . alternatePad) $ odds netlines
   in interleave es os

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

trows =
  let offsets = [0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5, 0]
      scale = reverse [0 .. 11]
      scales = repeat scale
   in zipWith (\o s -> (rotate o s)) offsets scales

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
main = do
  doDots [("ex1", graphToDot ex1Params ex1)]

--   doDots
--     [ ("ex3", ex3)
--     ]