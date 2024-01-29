module Main where

import ChordGraph
import System.Random
  ( Random (randomR),
    RandomGen,
    getStdGen,
    initStdGen,
  )

main :: IO ()
main =
  do
    gen <- initStdGen
    let cmajor = ["C", "E", "G"]
        (randomTransforms, _) = fisherYates gen transforms
        (randomTransformNames, _) = fisherYates gen transformNames
        path = take 4 randomTransforms
        progression = cmajor : findChordProgression cmajor randomTransforms -- fails
    printFlat progression
