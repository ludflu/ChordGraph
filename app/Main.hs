module Main where

import ChordGraph
import System.Random
  ( Random (randomR),
    RandomGen,
    mkStdGen,
    randomRIO,
  )

main :: IO ()
main = do
  let gen = mkStdGen 321 -- 123
      cmajor = ["C", "E", "G"]
      randomTransforms = [pl, slide, rp, prl]
      -- (randomTransforms, _) = fisherYates gen transforms
      -- (randomTransformNames, _) = fisherYates gen transformNames
      path = take 4 randomTransforms
  -- pathNames = take 4 randomTransformNames
  -- print pathNames
  -- let progression = cmajor : findChordProgression cmajor [pr, pl] -- fails
  let progression = cmajor : findChordProgression cmajor [p, r, l] -- fails
  printFlat progression
