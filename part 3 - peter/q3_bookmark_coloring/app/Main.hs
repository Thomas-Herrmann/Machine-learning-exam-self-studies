module Main where

import Coloring


main :: IO ()
main = do
    writeFile "eg_orig.dot" $ show (convertGtoVG exampleGraph)
    putStrLn "fbca for u4:" 
    let fbcaScores = pqToMap $ fbca exampleGraph 6 100.0 10
    print fbcaScores
    writeFile "eg_fbca.dot" $ show (addScores (convertGtoVG exampleGraph) fbcaScores)
    putStrLn "lfbca for u4:"
    let lfbcaScores = pqToMap $ lfbca exampleGraph 6 100.0 0.5 10
    let lfbcag = lfbcaGraph exampleGraph 6 100.0 0.5
    print lfbcaScores
    writeFile "eg_lfbca.dot" $ show (addScores (convertGtoVG lfbcag) lfbcaScores)
