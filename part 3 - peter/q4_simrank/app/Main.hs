module Main where

import SimRank

main :: IO ()
main = do 
    putStrLn "homogenous simrank(webG):"
    let wgsr = homoSimRank 0.8 webGraph 15
    print wgsr
    writeFile "g.dot" $ show webGraph
    writeFile "gsquared.dot" $ show (addScores (squareG webGraph) wgsr)
    putStrLn "bipartite simrank(personItemG):"
    let pigsr = bipartiteSimRank 0.8 personItemGraph 15
    print $ pigsr
    writeFile "bp.dot" $ show personItemGraph
    writeFile "bpsquared.dot" $ show (addScores (squareG personItemGraph) pigsr)